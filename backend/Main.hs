{-# LANGUAGE DeriveGeneric,  CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
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
import Agda.Interaction.InteractionTop
import Agda.Interaction.Highlighting.Precise hiding (String)
import Agda.Interaction.Highlighting.Range
import Agda.Utils.FileName
import qualified Agda.Syntax.Concrete as SC
import qualified Agda.Syntax.Common as C

data ClientProtocol
    = ByeBye
    | Typecheck Text
  deriving Show

$(deriveJSON id ''ClientProtocol)

data ServerProtocol = ErrMsg String | Response Response

#define JSON(t) $(deriveToJSON id ''t)
#define JSON_ignore(t) instance ToJSON t where; toJSON _ = Null

instance ToJSON Doc where
    toJSON = String . T.pack . render

instance ToJSON Range where
    toJSON (Range l u) = toJSON [l,u]

JSON_ignore(ModuleToSource)
JSON_ignore(AbsolutePath)
JSON_ignore(SC.Expr)

JSON(ServerProtocol)
JSON(Response)
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
main = runServer "0.0.0.0" 8000 handle

handle :: Request -> WebSockets Hybi10 ()
handle rq = do
    acceptRequest rq
    spawnPingThread 1
    sink <- getSink
    mq <- liftIO $ newTQueueIO
    void $ liftIO $ forkIO $ interaction sink mq
    forever (maybe (return ()) (liftIO . atomically . writeTQueue mq) =<< receiveJSON)
        `catchWsError`
       \ _ -> liftIO $ atomically $ writeTQueue mq ByeBye

receiveJSON :: FromJSON a => WebSockets Hybi10 (Maybe a)
receiveJSON = fmap decode receiveData

sendJSON :: ToJSON a => Sink Hybi10 -> a -> IO ()
sendJSON s = sendSink s . DataMessage . Text . encode

interaction :: Sink Hybi10 -> TQueue ClientProtocol -> IO ()
interaction sink mq = catchImp $ void $ runTCM $ catchTCM $ do
    setInteractionOutputCallback (liftIO . sendJSON sink . Response)
    let file = "/tmp/Test.agda" -- TODO: make a new name for each client
    evalStateT (loop file) initCommandState
  where
    catchTCM m = catchError m      (liftIO . sendJSON sink . ErrMsg <=< prettyError)
    catchImp m = catchImpossible m (sendJSON sink . ErrMsg . show)

    loop :: FilePath -> CommandM ()
    loop file = do
        msg <- liftIO (atomically (readTQueue mq))
        case msg of
            ByeBye -> return ()
            Typecheck t -> do
                liftIO $ T.writeFile file t
                runInteraction (IOTCM file NonInteractive Direct (Cmd_load file []))
                loop file

