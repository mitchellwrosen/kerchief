module Handler.Print (handlePrint) where

import Card          (showCard)
import Deck          (deckCards)
import Handler.Utils (printNoDeckLoadedError)
import Kerchief
import Utils         (io, printNumberedWith)

handlePrint :: [String] -> Kerchief ()
handlePrint [] = getDeck >>= maybe printNoDeckLoadedError (io . printNumberedWith showCard . deckCards)
handlePrint _  = printNoDeckLoadedError
