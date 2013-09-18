module Main where

import Control.Monad.State
import Control.Monad.Error
-- import Control.Applicative

-- import Data.Maybe
-- import Data.List
-- import Data.Map (Map)
-- import qualified Data.Map as Map

import System.Exit
import Agda.Interaction.GhcTop (mimicGHCi)

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Errors

import Agda.Utils.Impossible

-- | Main
main :: IO ()
main = do
    r <- runTCM $ mimicGHCi `catchError` \err -> do
      s <- prettyError err
      liftIO $ putStrLn s
      throwError err
    case r of
      Right _ -> exitSuccess
      Left _  -> exitFailure
  `catchImpossible` \e -> do
    putStr $ show e
    exitFailure

