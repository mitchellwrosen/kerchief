{-# LANGUAGE TemplateHaskell #-}

module Deck where

import Control.Lens
import Control.Monad
import Data.Time.Clock

import Richards

data Card = Card
    { cardFront       :: String
    , cardBack        :: String
    , cardScore       :: Int
    , cardLastStudied :: UTCTime
    }

data Deck = Deck
    { _deckName           :: String
    , _deckUnstudiedCards :: [Card]
    , _deckStudiedCards   :: [Card]
    }

makeLenses ''Deck

instance Eq Card where
    (Card f1 b1 _ _) == (Card f2 b2 _ _) = f1 == f2 && b1 == b2

shouldBeStudied :: Card -> IO Bool
shouldBeStudied (Card _ _ score lastStudied) = do
    now <- getCurrentTime
    return $ diffUTCTime now lastStudied > intervalAt score

-- | Update a deck to reflect the current time, by moving the appropriate cards
-- from deckStudiedCards to deckUnstudiedCards.
updateDeck :: Deck -> IO Deck
updateDeck deck = do
    (ts,fs) <- partitionM shouldBeStudied (deck ^. deckStudiedCards)
    return $ deck &
        (deckUnstudiedCards %~ (++ ts)) .
        (deckStudiedCards   .~ fs)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM (select p) ([],[])

select :: Monad m => (a -> m Bool) -> ([a],[a]) -> a -> m ([a],[a])
select p (ts,fs) x =
    ifM (p x)
        (return ((x:ts),fs))
        (return (ts,(x:fs)))

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f
