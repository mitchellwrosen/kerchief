module Handler.Utils where

import Control.Lens        ((^.))
import Control.Monad       (when)
import Control.Monad.Trans (liftIO)

import Deck     (deckName)
import Kerchief (Kerchief, getDeck, isModified, saveDeck)
import Utils

-- | Prompt to save the current deck, if there is one, and if it's been modified.
promptSaveCurrentDeck :: Kerchief ()
promptSaveCurrentDeck = getDeck >>= whenJust f
  where
    f deck = do
        let name = deck^.deckName
        modified <- isModified
        when modified $ do
            askYesNo ("Save deck \"" ++ name ++ "\"? (y/n) ")
                     (saveDeck >> liftIO (putStrLn $ "\"" ++ name ++ "\" saved."))
                     (return ())
