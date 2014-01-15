module Handler.Print (handlePrint) where

import Control.Monad.Trans (liftIO)

import Card          (showCard)
import Deck          (deckCards, newDeck)
import Handler.Ls    (handleLs)
import Handler.Utils (promptSaveCurrentDeck)
import Kerchief
import Utils         (askYesNo, printNumberedWith, unless')

handlePrint :: [String] -> Kerchief ()
handlePrint [] = getDeck >>= liftIO . maybe (putStrLn "No deck loaded. Try \"deck --help\".")
                                           (printNumberedWith showCard . deckCards)
handlePrint _  = liftIO $ putStrLn "Usage: print"
