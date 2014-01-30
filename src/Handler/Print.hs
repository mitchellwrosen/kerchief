module Handler.Print (handlePrint) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Card          (showCard)
import Deck          (Deck, deckCards, dueRatio)
import Handler.Utils (printNoDeckLoadedError)
import Kerchief      (Kerchief, getDeck)
import Utils         (printNumberedWith)

handlePrint :: [String] -> Kerchief ()
handlePrint [] = getDeck >>= maybe printNoDeckLoadedError doPrint
handlePrint _  = printNoDeckLoadedError

doPrint :: Deck -> Kerchief ()
doPrint deck = do
    let (numDue, totalCards) = dueRatio deck
    putStrLn $ show numDue ++ "/" ++ show totalCards ++ " cards due"
    printNumberedWith showCard . deckCards $ deck
