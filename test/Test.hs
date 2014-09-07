{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#define serialize(t) deriving instance Generic t; instance Binary t
#define ignore(t) instance Binary t where { get = return (error $ "Ignoring type ##t##") ; put _ = return () }

module Main where

import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C

import Control.Monad

import qualified Data.Map as M

import Text.PrettyPrint (Doc)

import Haste
import GHC.Generics (Generic)

import Agda.Interaction.Response
import qualified Agda.Syntax.Common as C
import qualified Agda.Syntax.Concrete as SC
import Agda.Interaction.Highlighting.Precise as P hiding (String)
import Agda.Interaction.Highlighting.Precise (Aspect(..))
import Agda.Interaction.Highlighting.Range
import Agda.TypeChecking.ModuleToSource
import Agda.Utils.FileName (AbsolutePath)

#ifndef __HASTE__
import "monads-tf" Control.Monad.State (evalStateT)

import System.IO

import Agda.Utils.Monad
import Agda.Utils.Pretty
import Agda.Utils.String

import Agda.Syntax.Common

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Monad.Base

import qualified Agda.Interaction.Options as O

import Agda.Interaction.InteractionTop
import Agda.Interaction.Response as R
import Agda.Interaction.EmacsCommand
import Agda.Interaction.Highlighting.Emacs
#endif

#ifdef __HASTE__
instance Show SC.Expr where show _ = "<expr>"
#endif


deriving instance Show Response
deriving instance Show GiveResult
deriving instance Show MakeCaseVariant
deriving instance Show Status
serialize(Response)
serialize(GiveResult)
serialize(C.InteractionId)
serialize(MakeCaseVariant)
serialize(Status)
serialize(SC.TopLevelModuleName)
serialize(P.MetaInfo)
serialize(OtherAspect)
serialize(CompressedFile)
serialize(NameKind)
serialize(C.Induction)
serialize(DisplayInfo)
serialize(Range)
serialize(P.Aspect)

instance Binary Bool
instance Binary Integer where
  get = fmap read get
  put = put . show

instance (Ord k,Binary k,Binary v) => Binary (M.Map k v) where
  get = fmap M.fromList get
  put = put . M.toList

ignore(AbsolutePath)
ignore(SC.Expr)
ignore(Doc)

type State = C.MVar Response

hello :: Server State -> String -> Server () -- Remote Response)
#ifdef __HASTE__
hello ovar txt = undefined
#else
hello ovar' txt = do
  ovar <- ovar'
  liftIO $ C.forkIO $ void $ runTCMTop $ do
    setInteractionOutputCallback $ \ r -> liftIO $ C.putMVar ovar r
    evalStateT
      (setCommandLineOptions' opts >> loop txt)
      (initCommandState { optionsOnReload = opts })
  return ()
 where
  opts = O.defaultOptions { O.optIncludeDirs = Left ["~/build/lib/src","/tmp"] }

  loop :: String -> CommandM ()
  loop txt = do
    let file = "/tmp/Test.agda"
    liftIO $ writeFile file txt
    runInteraction (IOTCM file NonInteractive Direct (Cmd_load file []))
#endif

listen :: Server State -> Server Response
listen ovar' = do
  ovar <- ovar'
  liftIO $ C.takeMVar ovar

main = do
  runApp (mkConfig "ws://localhost:8000" 8000) $ do

    ovar <- liftServerIO $ C.newEmptyMVar

    api_hello <- remote (hello ovar)
    api_listen <- remote (listen ovar)

    runClient $ do
      onServer (api_hello <.> "id : {A : Set} -> A -> A\nid x = x\n")
      forever $ do
        resp <- onServer api_listen
        alert (mini_show resp)

mini_show :: Response -> String
mini_show k = case k of
     Resp_HighlightingInfo{} -> "Resp_HighlightingInfo"
     Resp_Status{} -> "Resp_Status"
     Resp_JumpToError{} -> "Resp_JumpToError"
     Resp_InteractionPoints{} -> "Resp_InteractionPoints"
     Resp_GiveAction{} -> "Resp_GiveAction"
     Resp_MakeCase{} -> "Resp_MakeCase"
     Resp_SolveAll{} -> "Resp_SolveAll"
     Resp_DisplayInfo{} -> "Resp_DisplayInfo"
     Resp_RunningInfo{} -> "Resp_RunningInfo"
     Resp_ClearRunningInfo{} -> "Resp_ClearRunningInfo"
     Resp_ClearHighlighting{} -> "Resp_ClearHighlighting"
