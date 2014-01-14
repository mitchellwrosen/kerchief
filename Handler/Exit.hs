{-# LANGUAGE LambdaCase #-}

module Handler.Exit (handleExit) where

import Control.Lens
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable       (traverse_)
import System.Exit         (exitSuccess)
import Text.Printf

import Deck
import Kerchief
import Utils (askYesNo)

handleExit :: [String] -> Kerchief ()
handleExit [] = do
    getLoadedDecks >>= liftIO . traverse_ f
    liftIO exitSuccess
  where
    -- TODO: Would it be useful to prompt "are you sure?" if "n"?
    f :: Deck -> IO ()
    f deck = askYesNo (printf "Save deck \"%s\"? (y/n) " (deck^.deckName))
                      (writeDeck deck)
                      (return ())
handleExit _ = liftIO $ putStrLn "Usage: exit"
