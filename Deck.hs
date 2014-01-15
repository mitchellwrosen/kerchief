{-# LANGUAGE TemplateHaskell #-}

module Deck where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Extras   (partitionM)
import           Data.Foldable          (Foldable, foldMap)
import           Data.Monoid            -- ((<>))
import           Data.Serialize         (Serialize, get, put)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           System.IO

import Card

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

-- | Add a card to the deck, only if it doesn't already exist.
addCard :: Deck -> Card -> Deck
addCard deck card =
    if S.member card dueCards || S.member card doneCards
        then deck
        else deck & deckDueCards %~ S.insert card
  where
    dueCards, doneCards :: Set Card
    dueCards  = deck ^. deckDueCards
    doneCards = deck ^. deckDoneCards

-- | Convenience method, combination of newCard and addCard.
addNewCard :: String -> String -> Deck -> IO Deck
addNewCard front back deck = addCard deck <$> newCard front back

-- | Convenience method, combination of newTwoWayCard and addCard.
addNewTwoWayCard :: String -> String -> Deck -> IO Deck
addNewTwoWayCard front back deck = do
    (c1,c2) <- newTwoWayCard front back
    return $ addCard (addCard deck c1) c2

-- | Remove a card from the deck.
removeCard :: Card -> Deck -> Deck
removeCard card deck =
    if S.member card (deck ^. deckDueCards)
        then deck & deckDueCards %~ S.delete card
        else deck & deckDoneCards %~ S.delete card

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
deckCards (Deck _ x y) = x <> y

-- | Search a deck's cards (both front and back) for a specific string.
searchDeck :: String -> Deck -> Set Card
searchDeck str = S.filter (containsText str) . deckCards

-- | Given a (foldable) container of Decks, return the first Deck with name
-- matching the input String.
getDeckWithName :: Foldable t => String -> t Deck -> Maybe Deck
getDeckWithName name = getFirst . foldMap f
  where
    f deck@(Deck name' _ _)
        | name == name' = First (Just deck)
        | otherwise     = First Nothing
