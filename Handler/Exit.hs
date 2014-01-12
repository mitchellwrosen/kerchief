{-# LANGUAGE LambdaCase #-}

module Handler.Exit (handleExit) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Exit         (exitSuccess)

import Deck
import Kerchief

handleExit :: [String] -> Kerchief ()
handleExit ["--help"] = liftIO printExitUsage
handleExit ["y"]      = saveDeck >> liftIO exitSuccess
handleExit ["n"]      = liftIO exitSuccess
handleExit []         = getDeck >>= maybe noDeck yesDeck
  where
    noDeck :: Kerchief ()
    noDeck = liftIO (putStrLn "Bye.")

    yesDeck :: Deck -> Kerchief ()
    yesDeck (Deck name _ _) = askYesNo ("Save deck \"" ++ name ++ "\"? (y/n)") yesSave noSave

    yesSave, noSave, yesSure, noSure :: Kerchief ()
    yesSave = saveDeck >> liftIO (putStrLn "Deck saved. Bye.") >> liftIO exitSuccess
    noSave  = askYesNo "Are you sure? (y/n)" yesSure noSure
    yesSure = liftIO (putStrLn "Deck not saved. Bye.") >> liftIO exitSuccess
    noSure  = handleExit []
handleExit _          = liftIO printExitUsage

askYesNo :: MonadIO m => String -> m a -> m a -> m a
askYesNo s yes no = do
    liftIO (putStrLn s)
    liftIO getLine >>= \case
        "y"   -> yes
        "Y"   -> yes
        "yes" -> yes
        "n"   -> no
        "N"   -> no
        "no"  -> no
        _     -> liftIO (putStrLn "Please input \"y\" or \"n\".") >> askYesNo s yes no

printExitUsage :: IO ()
printExitUsage = mapM_ putStrLn
    [ "Usage: exit [y|n]"
    , "exits, optionally saving the current deck (will always prompt)"
    ]
