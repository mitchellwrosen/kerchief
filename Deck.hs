{-# LANGUAGE TemplateHaskell #-}

module Deck where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Extras   (partitionM)
import Data.List              (delete)

import Card

data Deck = Deck
    { deckName      :: String
    , deckDueCards  :: TVar [Card]
    , deckDoneCards :: TVar [Card]
    } 

newDeck :: String -> IO Deck
newDeck name = Deck name <$> newTVarIO [] <*> newTVarIO []

-- | Update a deck to reflect the current time, by moving the appropriate cards
-- from done to due.
--
-- Not thread safe, because determining if a card is due for studying must be
-- in IO (getCurrentTime).
updateDeck :: Deck -> IO ()
updateDeck (Deck _ tdueCards tdoneCards) = do
    (ts,fs) <- readTVarIO tdoneCards >>= partitionM isDue
    atomically $ do
        modifyTVar tdueCards (++ts)
        writeTVar tdoneCards fs

-- | Add a card to the deck.
addCard :: Deck -> Card -> STM ()
addCard (Deck _ dueCards _) card = modifyTVar dueCards (card:)

-- | Convenience method, combination of newCard and addCard.
addNewCard :: String -> String -> Deck -> IO ()
addNewCard front back deck = newCard front back >>= atomically . addCard deck

-- | Convenience method, combination of newTwoWayCard and addCard.
addNewTwoWayCard :: String -> String -> Deck -> IO ()
addNewTwoWayCard front back deck = do
    (c1,c2) <- newTwoWayCard front back
    atomically $ do
        addCard deck c1
        addCard deck c2

-- | Study a card, which updates its timestamp and moves it from due to done.
--
-- Not thread safe, because updating a card's timestamp is in IO.
studyCard :: Feedback -> Card -> Deck -> IO ()
studyCard feedback card (Deck _ tdueCards tdoneCards) = do
    card' <- updateCard feedback card
    atomically $ do
        modifyTVar tdueCards (delete card') -- assumes card exists in due
        modifyTVar tdoneCards (card':)

printDeck :: Deck -> IO ()
printDeck (Deck name tdueCards tdoneCards) = do
    dueCards <- readTVarIO tdueCards
    doneCards <- readTVarIO tdoneCards
    print $ "Name: " ++ name ++ ", Due: " ++ show dueCards ++ ", Done: " ++ show doneCards
