{-# LANGUAGE TemplateHaskell #-}

module Deck where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Extras   (partitionM)
import           Data.Monoid            ((<>))
import           Data.Set               (Set)
import qualified Data.Set               as S

import Card

data Deck = Deck
    { _deckName      :: !String
    , _deckDueCards  :: Set Card
    , _deckDoneCards :: Set Card
    }
makeLenses ''Deck

newDeck :: String -> Deck
newDeck name = Deck name S.empty S.empty

-- | Update a deck to reflect the current time, by moving the appropriate cards
-- from done to due.
--
-- Super slow because of toList/fromList - it would be nice to use a
-- partitionM on Sets but the implementation is hidden.
--
-- Not thread safe, because determining if a card is due for studying must be
-- in IO (getCurrentTime).
updateDeck :: Deck -> IO Deck
updateDeck deck = do
    (ts,fs) <- partitionM isDue . S.toList $ deck ^. deckDueCards
    return $ deck
        & deckDueCards  %~ S.union (S.fromList ts)
        & deckDoneCards .~ S.fromList fs

-- | Add a card to the deck.
addCard :: Deck -> Card -> Deck
addCard deck card = deck & deckDueCards %~ S.insert card

-- | Convenience method, combination of newCard and addCard.
addNewCard :: String -> String -> Deck -> IO Deck
addNewCard front back deck = addCard deck <$> newCard front back

-- | Convenience method, combination of newTwoWayCard and addCard.
addNewTwoWayCard :: String -> String -> Deck -> IO Deck
addNewTwoWayCard front back deck = do
    (c1,c2) <- newTwoWayCard front back
    return $ addCard (addCard deck c1) c2

removeCard :: Deck -> Card -> Deck
removeCard = undefined

removeDueCard :: Deck -> Card -> Deck
removeDueCard = undefined

removeNthDueCard :: Deck -> Int -> Deck
removeNthDueCard = undefined

removeDoneCard :: Deck -> Card -> Deck
removeDoneCard = undefined

removeNthDoneCard :: Deck -> Int -> Deck
removeNthDoneCard = undefined

-- | Study a card, which updates its timestamp and moves it from due to done.
--
-- Not thread safe, because updating a card's timestamp is in IO.
studyCard :: Feedback -> Card -> Deck -> IO Deck
studyCard feedback card deck = do
    card' <- updateCard feedback card
    return $ deck
        & deckDueCards  %~ S.delete card' -- assumes card exists in due
        & deckDoneCards %~ S.insert card'

deckCards :: Deck -> Set Card
deckCards (Deck _ x y) = S.union x y

-- | Print cards, sorted alphabetically.
printDeckCards :: Deck -> IO ()
printDeckCards (Deck _ dueCards doneCards) = f $ S.toDescList (dueCards <> doneCards)
  where
    f :: [Card] -> IO ()
    f = mapM_ (\(n,c) -> putStrLn (show n ++ ". " ++ show c)) . zip ([1..] :: [Int])

-- | Search a deck's cards (both front and back) for a specific string.
searchDeck :: String -> Deck -> Set Card
searchDeck str = S.filter (containsText str) . deckCards
