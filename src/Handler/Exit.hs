module Handler.Exit (handleExit) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import System.Exit         (exitSuccess)

import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief            (Kerchief)

handleExit :: [String] -> Kerchief ()
handleExit [] = promptSaveCurrentDeck >> io exitSuccess
handleExit _ = putStrLn "Usage: exit"
