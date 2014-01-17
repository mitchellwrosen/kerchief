{-# LANGUAGE LambdaCase #-}

module Handler.Edit (handleEdit) where

import           Data.Foldable       (mapM_)
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Set.Extra      as S

import           Card                (Card, setCardContents, showCard)
import           Deck                (Deck, deckCards, modifyCard, searchDeck)
import           Kerchief            (Kerchief, getDeck, modifyDeck)
import           Utils               (askYesNo, io, printNumberedWith, reads')

import Prelude hiding (mapM_)

handleEdit :: [String] -> Kerchief ()
handleEdit ["--help"]  = io printEditUsage
handleEdit [word]      = maybe (handleEditWord word) handleEditIndex (reads' word)
handleEdit _           = io printEditUsage

printEditUsage :: IO ()
printEditUsage = mapM_ putStrLn
    [ "Usage: edit [word|index]"
    , "edit word: edit a card containing |word| from the current deck"
    , "edit index: edit card at |index| from the current deck (see \"print\")"
    ]

handleEditWord :: String -> Kerchief ()
handleEditWord word = getDeck >>= maybe noDeck yesDeck
  where
    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = do
        let cards = searchDeck word deck
        if S.null cards
            then io $ putStrLn "No matching cards found."
            else loop cards
      where
        -- "loop" only so long as the user is inputting bad data.
        loop :: Set Card -> Kerchief ()
        loop cards = do
            io $ putStrLn "Matching cards found:"
            io $ printNumberedWith showCard cards
            io $ putStr "Edit which card? (\"-\" to go back, \"all\" to edit all) "
            io getLine >>= \case
                "-"   -> io $ putStrLn "No cards editd."
                "all" -> editAll cards
                s     -> maybe (io (putStrLn "Please pick a valid integer.") >> loop cards)
                               promptContents
                               (reads' s >>= \n -> S.safeElemAt (n-1) cards)

-- index supplied is 1-based
handleEditIndex :: Int -> Kerchief ()
handleEditIndex n = getDeck >>= maybe noDeck promptIndex
  where
    promptIndex :: Deck -> Kerchief ()
    promptIndex deck = maybe (io $ putStrLn "Please pick a valid integer.")
                             promptContents
                             (S.safeElemAt (n-1) $ deckCards deck)

noDeck :: Kerchief ()
noDeck = io $ putStrLn "No deck loaded. Try \"load --help\"."

promptContents :: Card -> Kerchief ()
promptContents card = do
    io . putStrLn $ showCard card

    io $ putStr "Front: "
    newFront <- io getLine

    io $ putStr "Back: "
    newBack <- io getLine

    askYesNo "Really edit card? (y/n) "
             (doEditCard card newFront newBack)
             (io $ putStrLn "Card not edited.")

doEditCard :: Card -> String -> String -> Kerchief ()
doEditCard card front back = do
    modifyDeck $ modifyCard card $ setCardContents front back
    io $ putStrLn "Card edited."

editAll :: Set Card -> Kerchief ()
editAll = mapM_ promptContents
