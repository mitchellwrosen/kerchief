module Handler.Utils where

import Control.Lens        ((^.))
import Control.Monad       (when)

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
        when modified $
            askYesNo ("Save deck \"" ++ name ++ "\"? (y/n) ")
                     (saveDeck >> io (putStrLn $ "\"" ++ name ++ "\" saved."))
                     (return ())

printNoDeckLoadedError :: Kerchief ()
printNoDeckLoadedError = io $ putStrLn "No deck loaded. Try \"load --help\"."
