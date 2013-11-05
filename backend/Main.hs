{-# LANGUAGE DeriveGeneric,  CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Text.PrettyPrint

import Data.Aeson
import Data.Aeson.TH

import Control.Monad.State
import Control.Monad.Error
-- import Control.Applicative

-- import Data.Maybe
-- import Data.List
-- import Data.Map (Map)
-- import qualified Data.Map as Map
--
import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.WebSockets

import Control.Concurrent (forkIO)

import Agda.Interaction.FindFile

import Agda.TypeChecking.Monad hiding (MetaInfo)
import Agda.TypeChecking.Errors

import Agda.Utils.Impossible

import Control.Concurrent.STM

import Agda.Interaction.Response
import qualified Agda.Interaction.BasicOps as B
import Agda.Interaction.InteractionTop
import Agda.Interaction.Highlighting.Precise hiding (String)
import Agda.Interaction.Highlighting.Range
import Agda.Utils.FileName
import Agda.Syntax.Position (noRange)
import qualified Agda.Syntax.Concrete as SC
import qualified Agda.Syntax.Common as C

data ClientProtocol
    = ByeBye
    | Typecheck       { txt :: Text }
    | Goal            { ip :: Int }
    | Give            { ip :: Int, txt :: Text }
    | GoalAndInferred { ip :: Int, txt :: Text }
    | Case            { ip :: Int, txt :: Text }
    | Auto            { ip :: Int, txt :: Text }
    | Refine          { ip :: Int, txt :: Text }
    | Normalise       { ip :: Int, txt :: Text }
  deriving Show

data ServerProtocol = ServerError String | Response Response

instance ToJSON ServerProtocol where
    toJSON (Response r)    = toJSON r
    toJSON (ServerError s) = object
        [ T.pack "tag" .= String (T.pack "ServerError")
        , T.pack "contents" .= String (T.pack s)
        ]

$(deriveFromJSON defaultOptions ''ClientProtocol)

#define JSON(t) $(deriveToJSON defaultOptions ''t)
#define JSON_ignore(t) instance ToJSON t where; toJSON _ = Null

instance ToJSON Doc where
    toJSON = String . T.pack . render

instance ToJSON Range where
    toJSON (Range l u) = toJSON [l,u]

JSON_ignore(ModuleToSource)
JSON_ignore(AbsolutePath)
JSON_ignore(SC.Expr)

$(deriveToJSON defaultOptions { constructorTagModifier = drop 5 } ''Response)
                                -- drop Resp_ from Response constructors

JSON(GiveResult)
JSON(InteractionId)
JSON(Status)
JSON(SC.TopLevelModuleName)
JSON(MetaInfo)
JSON(Aspect)
JSON(OtherAspect)
JSON(CompressedFile)
JSON(NameKind)
JSON(C.Induction)
JSON(DisplayInfo)

main :: IO ()
main = do
    ir <- newTVarIO (0 :: Int)
    runServer "0.0.0.0" 8000 (handle ir)

handle :: TVar Int -> Request -> WebSockets Hybi10 ()
handle ir rq = do
    me <- liftIO $ atomically $ do
        v <- readTVar ir
        modifyTVar ir succ
        return v
    acceptRequest rq
    spawnPingThread 1
    sink <- getSink
    mq <- liftIO $ newTQueueIO
    void $ liftIO $ forkIO $ interaction me sink mq
    let body = do
            d <- receiveData
            liftIO $ case decode d of
                Just msg -> atomically (writeTQueue mq msg)
                Nothing  -> sendJSON sink (ServerError $ "Cannot parse: " ++ show d)
    forever body `catchWsError` \ _ -> liftIO $ atomically $ writeTQueue mq ByeBye

receiveJSON :: FromJSON a => WebSockets Hybi10 (Maybe a)
receiveJSON = fmap decode receiveData

sendJSON :: ToJSON a => Sink Hybi10 -> a -> IO ()
sendJSON s = sendSink s . DataMessage . Text . encode

toCmd :: FilePath -> ClientProtocol -> IO (Maybe Interaction)
toCmd _    ByeBye           = return Nothing
toCmd file (Typecheck{txt}) = do
    T.writeFile file txt
    return $ Just $ Cmd_load file []
toCmd _    cl = return $ Just $ case cl of
    Goal{}            -> Cmd_goal_type_context B.Normalised ip' noRange ""
    GoalAndInferred{} -> Cmd_goal_type_context_infer B.Normalised ip' noRange txt'
    Give{}            -> Cmd_give ip' noRange txt'
    Case{}            -> Cmd_make_case ip' noRange txt'
    Auto{}            -> Cmd_auto ip' noRange txt'
    Refine{}          -> Cmd_refine_or_intro False {- assume not in a pattern-matching lambda -}
                                   ip' noRange txt'
    Normalise{}       -> Cmd_compute False {- don't ignore abstract or now -}
                                   ip' noRange txt'
    ByeBye{}          -> error "impossible"
    Typecheck{}       -> error "impossible"
  where
    ip' = read (show (ip cl))
    txt' = T.unpack (txt cl)

interaction :: Int -> Sink Hybi10 -> TQueue ClientProtocol -> IO ()
interaction me sink mq = catchImp $ void $ runTCM $ catchTCM $ do
    modify $ \ st -> st { stInteractionOutputCallback = liftIO . sendJSON sink . Response }
    let dir  = "/tmp/" ++ show me
        file = dir ++ "/Test.agda"
    liftIO $ createDirectoryIfMissing True dir
    msg $ "Serving " ++ file
    evalStateT (unCommandM $ loop file) initCommandState
  where
    msg :: MonadIO m => String -> m ()
    msg = liftIO . putStrLn . (show me ++) . (":" ++)

    catchTCM m = catchError m      (liftIO . sendJSON sink . ServerError <=< prettyError)
    catchImp m = catchImpossible m (sendJSON sink . ServerError . show)

    loop :: FilePath -> CommandM ()
    loop file = do
        m_cmd <- liftIO (toCmd file =<< atomically (readTQueue mq))
        case m_cmd of
            Nothing -> return ()
            Just cmd -> do
                msg "got command"
                runInteraction (IOTCM file NonInteractive Direct cmd)
                msg "command finished"
                loop file

