{-# LANGUAGE LambdaCase #-}

module Handler.Remove (handleRemove) where

import           Data.Foldable       (mapM_)
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Set.Extra      as S

import           Card                (Card, showCard)
import           Deck                (Deck, deckCards, removeCard, searchDeck)
import           Handler.Utils       (printNoDeckLoadedError)
import           Kerchief            (Kerchief, getDeck, modifyDeck)
import           Utils               (io, printNumberedWith, reads')

import Prelude hiding (mapM_)

handleRemove :: [String] -> Kerchief ()
handleRemove ["--help"]  = io printRemoveUsage
handleRemove [word]      = maybe (handleRemoveWord word) handleRemoveIndex (reads' word)
handleRemove _           = io printRemoveUsage

printRemoveUsage :: IO ()
printRemoveUsage = mapM_ putStrLn
    [ "Usage: remove [word|index]"
    , "remove word: remove a card containing |word| from the current deck"
    , "remove index: remove card at |index| from the current deck (see \"print\")"
    ]

handleRemoveWord :: String -> Kerchief ()
handleRemoveWord word = getDeck >>= maybe printNoDeckLoadedError yesDeck
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
            io $ putStr "Remove which card? (\"-\" to go back, \"all\" to remove all) "
            io getLine >>= \case
                "-"   -> io $ putStrLn "No cards removed."
                "all" -> removeAll cards
                s     -> maybe (io (putStrLn "Please pick a valid integer.") >> loop cards)
                               doRemoveCard
                               (reads' s >>= \n -> S.safeElemAt (n-1) cards)

-- index supplied is 1-based
handleRemoveIndex :: Int -> Kerchief ()
handleRemoveIndex n = getDeck >>= maybe printNoDeckLoadedError yesDeck
  where
    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = maybe (io $ putStrLn "Please pick a valid integer.")
                         doRemoveCard
                         (S.safeElemAt (n-1) $ deckCards deck)

doRemoveCard :: Card -> Kerchief ()
doRemoveCard card = do
    modifyDeck $ removeCard card
    io $ putStrLn "Card removed."

removeAll :: Set Card -> Kerchief ()
removeAll = mapM_ doRemoveCard
