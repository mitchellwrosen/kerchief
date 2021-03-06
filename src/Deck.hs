{-# LANGUAGE TemplateHaskell #-}

module Deck where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid            ((<>))
import           Data.Serialize         (Serialize, get, put)
import           Data.Set               (Set)
import qualified Data.Set               as S

import Card
import qualified Data.Set.Extra         as S
import Utils                            (partitionM)

data Deck = Deck
    { _deckName      :: !String
    , _deckDueCards  :: Set Card
    , _deckDoneCards :: Set Card
    } deriving Show
makeLenses ''Deck

-- Compare decks only on the name.
instance Eq Deck where
    (Deck n1 _ _) == (Deck n2 _ _) = n1 == n2

instance Ord Deck where
    compare (Deck n1 _ _) (Deck n2 _ _) = compare n1 n2

instance Serialize Deck where
    put (Deck name due done) = put name >> put due >> put done
    get = Deck <$> get <*> get <*> get

emptyDeck :: String -> Deck
emptyDeck name = Deck name S.empty S.empty

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
    (ts,fs) <- partitionM isDue . S.toList $ deck^.deckDoneCards
    return $ deck
        & deckDueCards  %~ S.union (S.fromList ts)
        & deckDoneCards .~ S.fromList fs

-- | Add a card to the deck, only if it doesn't already exist.
addCard :: Card -> Deck -> Deck
addCard card deck =
    if S.member card dueCards || S.member card doneCards
        then deck
        else deck & deckDueCards %~ S.insert card
  where
    dueCards, doneCards :: Set Card
    dueCards  = deck ^. deckDueCards
    doneCards = deck ^. deckDoneCards

-- | Convenience method, combination of newCard and addCard.
addNewCard :: String -> String -> Maybe String -> Deck -> IO Deck
addNewCard front back soundUrl  deck = flip addCard deck <$> newCard front back soundUrl

-- | Remove a card from the deck.
removeCard :: Card -> Deck -> Deck
removeCard card deck =
    if S.member card (deck ^. deckDueCards)
        then deck & deckDueCards  %~ S.delete card
        else deck & deckDoneCards %~ S.delete card

modifyCard :: Card -> (Card -> Card) -> Deck -> Deck
modifyCard card f deck 
    | S.member card (deck^.deckDueCards)  = deck & deckDueCards  %~ S.modify card f
    | S.member card (deck^.deckDoneCards) = deck & deckDoneCards %~ S.modify card f
    | otherwise = deck

-- | Study a card, which updates its timestamp and moves it from due to done.
-- Not thread safe, because updating a card's timestamp is in IO.
studyCard :: Feedback -> Card -> Deck -> IO Deck
studyCard feedback card deck = flip studyCard' deck <$> updateCard feedback card

-- | Variant of studyCard that takes an already-updated card and moves it from
-- due to done.
studyCard' :: Card -> Deck -> Deck
studyCard' card deck = deck & deckDueCards  %~ S.delete card -- assumes card exists in due
                            & deckDoneCards %~ S.insert card

deckCards :: Deck -> Set Card
deckCards (Deck _ x y) = x <> y

-- | Search a deck's cards (both front and back) for a specific string.
searchDeck :: String -> Deck -> Set Card
searchDeck str = S.filter (containsText str) . deckCards

-- | Convenience method to get the due/total counts.
dueRatio :: Deck -> (Int,Int)
dueRatio deck = (S.size (deck^.deckDueCards), S.size (deckCards deck))
