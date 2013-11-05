{-# LANGUAGE DeriveGeneric,  CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Text.PrettyPrint

import Data.Aeson
import Data.Aeson.TH

import Control.Monad.State
import Control.Monad.Error

import qualified Control.Exception as E

import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.WebSockets hiding (Response)

import Control.Concurrent.STM

import Agda.Interaction.FindFile
import Agda.Interaction.Highlighting.Precise hiding (String)
import Agda.Interaction.Highlighting.Range
import Agda.Interaction.InteractionTop
import Agda.Interaction.Response
import qualified Agda.Interaction.BasicOps as B

import Agda.Syntax.Position (noRange)
import qualified Agda.Syntax.Common as C
import qualified Agda.Syntax.Concrete as SC

import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad hiding (MetaInfo)
import Agda.Utils.FileName
import Agda.Utils.Impossible

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


----------------------------------------------------------------
-- Main

main :: IO ()
main = do
    ir <- newTVarIO (0 :: Int)
    runServer "0.0.0.0" 8000 (handle ir)

handle :: TVar Int -> PendingConnection -> IO ()
handle ir pc = do
    conn <- acceptRequest pc

    me <- liftIO $ atomically $ do
        v <- readTVar ir
        modifyTVar ir succ
        return v

    interaction me conn
        `E.catch` \ ConnectionClosed -> return ()
        `E.catch` \ (_ :: HandshakeException) -> return ()

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

interaction :: Int -> Connection -> IO ()
interaction me conn = catchImp $ void $ runTCM $ catchTCM $ do
    modify $ \ st -> st { stInteractionOutputCallback = liftIO . sendJSON conn . Response }
    liftIO $ createDirectoryIfMissing True dir
    msg $ "Serving " ++ file
    evalStateT (unCommandM loop) initCommandState
  where
    dir  = "/tmp/" ++ show me
    file = dir ++ "/Test.agda"

    msg :: MonadIO m => String -> m ()
    msg = liftIO . putStrLn . (show me ++) . (":" ++)

    catchTCM m = catchError m      (liftIO . sendJSON conn . ServerError <=< prettyError)
    catchImp m = catchImpossible m (sendJSON conn . ServerError . show)

    loop :: CommandM ()
    loop = do
        m_cmd <- liftIO (toCmd file =<< recv)
        case m_cmd of
            Nothing -> msg "Terminating"
            Just cmd -> do
                msg "Serving command"
                runInteraction (IOTCM file NonInteractive Direct cmd)
                msg "Command finished"
                loop

    recv :: FromJSON a => IO a
    recv = do
        md <- receiveJSON conn
        case md of
            Just x  -> return x
            Nothing -> msg "Got garbage" >> recv

-----------------------------------------------------------------
-- JSON utilities

receiveJSON :: FromJSON a => Connection -> IO (Maybe a)
receiveJSON = fmap decode . receiveData

sendJSON :: ToJSON a => Connection -> a -> IO ()
sendJSON c = sendTextData c . encode

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
