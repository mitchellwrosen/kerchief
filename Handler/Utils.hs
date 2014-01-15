module Handler.Utils where

import Control.Monad.Trans (liftIO)

import Deck
import Kerchief
import Utils

-- | Prompt to save the current deck, if there is one.
promptSaveCurrentDeck :: Kerchief ()
promptSaveCurrentDeck = getDeck >>= whenJust f
  where
    f (Deck name _ _) = 
        askYesNo ("Save deck \"" ++ name ++ "\"? (y/n) ")
                 (saveDeck >> liftIO (putStrLn $ "\"" ++ name ++ "\" saved."))
                 (return ())
