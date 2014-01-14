{-# LANGUAGE LambdaCase #-}

module Handler.Exit (handleExit) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Exit         (exitSuccess)
import Text.Printf

import Deck
import Kerchief
import Utils (askYesNo)

handleExit :: [String] -> Kerchief ()
handleExit ["--help"] = liftIO printExitUsage
handleExit ["y"]      = saveAndExit
handleExit ["n"]      = liftIO dontSaveAndExit
handleExit []         = getDeck >>= maybe noDeck yesDeck
  where
    noDeck :: Kerchief ()
    noDeck = liftIO $ putStrLn "Bye."

    yesDeck :: Deck -> Kerchief ()
    yesDeck (Deck name _ _) = askYesNo (printf "Save deck \"%s\"? (y/n) " name)
                                       saveAndExit
                                       (askYesNo "Are you sure? (y/n) "
                                                 (liftIO dontSaveAndExit)
                                                 (handleExit []))
handleExit _ = liftIO printExitUsage

printExitUsage :: IO ()
printExitUsage = mapM_ putStrLn
    [ "Usage: exit [y|n]"
    , "exits, optionally saving the current deck (will always prompt)"
    ]

saveAndExit :: Kerchief ()
saveAndExit = saveDeck >> liftIO (putStrLn "Deck saved. Bye." >> exitSuccess)

dontSaveAndExit :: IO ()
dontSaveAndExit = putStrLn "Deck not saved. Bye." >> exitSuccess
