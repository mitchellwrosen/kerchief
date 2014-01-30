module Handler.Print (handlePrint) where

import Kerchief.Prelude

import Card          (showCard)
import Deck          (deckCards)
import Handler.Utils (printNoDeckLoadedError)
import Kerchief      (Kerchief, getDeck)
import Utils         (printNumberedWith)

handlePrint :: [String] -> Kerchief ()
handlePrint [] = getDeck >>= maybe printNoDeckLoadedError (io . printNumberedWith showCard . deckCards)
handlePrint _  = printNoDeckLoadedError
