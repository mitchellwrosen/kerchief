module Handler.Utils where

import Kerchief.Prelude

import Deck     (deckName)
import Kerchief (Kerchief, getDeck, isModified, saveDeck)
import Utils

import Prelude hiding (putStrLn)

-- | Prompt to save the current deck, if there is one, and if it's been modified.
promptSaveCurrentDeck :: Kerchief ()
promptSaveCurrentDeck = getDeck >>= whenJust f
  where
    f deck = do
        let name = deck^.deckName
        modified <- isModified
        when modified $
            askYesNo ("Save deck \"" ++ name ++ "\"? (y/n) ")
                     (saveDeck >> putStrLn ("\"" ++ name ++ "\" saved."))
                     (return ())

printNoDeckLoadedError :: Kerchief ()
printNoDeckLoadedError = putStrLn "No deck loaded. Try \"load --help\"."
