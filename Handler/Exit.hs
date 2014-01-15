{-# LANGUAGE LambdaCase #-}

module Handler.Exit (handleExit) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Exit         (exitSuccess)

import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief

handleExit :: [String] -> Kerchief ()
handleExit [] = promptSaveCurrentDeck >> liftIO exitSuccess
handleExit _ = liftIO $ putStrLn "Usage: exit"
