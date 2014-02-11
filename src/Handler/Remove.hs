module Handler.Remove (handleRemove) where

import Kerchief.Prelude

import           Data.Foldable       (foldlM)
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Set.Extra      as S

import           Card                (Card, showCard)
import           Deck                (Deck, deckCards, removeCard, searchDeck)
import           Handler.Utils       (printNoDeckLoadedError)
import           Kerchief            (Kerchief, getDeck, setDeck)
import           Utils               (printNumberedWith, reads')

import Prelude hiding (getLine, putStr, putStrLn)

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
handleRemoveWord word = getDeck >>= maybe printNoDeckLoadedError handleRemoveWord'
  where
    handleRemoveWord' :: Deck -> Kerchief ()
    handleRemoveWord' deck = do
        let cards = searchDeck word deck
        if S.null cards
            then putStrLn "No matching cards found."
            else promptRemoveCards cards >>= setDeck . ($ deck)

-- loop only so long as the user is inputting bad data.
promptRemoveCards :: Set Card -> Kerchief (Deck -> Deck)
promptRemoveCards cards = do
    putStrLn "Matching cards found:"
    printNumberedWith showCard cards
    putStr "Remove which card? (\"-\" to go back, \"all\" to remove all) "
    getLine >>= \ms -> case ms of
        "-"   -> putStrLn "No cards removed." >> return id
        "all" -> foldlM (\f card -> (f .) <$> doRemoveCard card) id cards
        s     -> maybe (putStrLn "Please pick a valid integer." >> promptRemoveCards cards)
                       doRemoveCard
                       (reads' s >>= \n -> S.safeElemAt (n-1) cards)

-- index supplied is 1-based
handleRemoveIndex :: Int -> Kerchief ()
handleRemoveIndex n = getDeck >>= maybe printNoDeckLoadedError promptIndex
  where
    promptIndex :: Deck -> Kerchief ()
    promptIndex deck = promptIndex' >>= setDeck . ($ deck)
      where
        promptIndex' :: Kerchief (Deck -> Deck)
        promptIndex' = maybe (putStrLn "Please pick a valid integer." >> return id)
                             doRemoveCard
                             (S.safeElemAt (n-1) $ deckCards deck)

doRemoveCard :: Card -> Kerchief (Deck -> Deck)
doRemoveCard card = removeCard card <$ putStrLn "Card removed."
