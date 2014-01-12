{-# LANGUAGE TemplateHaskell #-}

module Card where

import Control.Arrow        ((&&&))
import Control.Applicative
import Control.Lens
import Data.Time.Clock

import Richards

data Feedback = Wrong | Hard | Easy

data Card = Card
    { _cardFront       :: String
    , _cardBack        :: String
    , _cardScore       :: Int
    , _cardLastStudied :: UTCTime
    }

makeLenses ''Card

-- | Compare cards on their contents, not score or timestamp.
instance Eq Card where
    (Card f1 b1 _ _) == (Card f2 b2 _ _) = f1 == f2 && b1 == b2

-- | Determine if a card is due for studying.
isDue :: Card -> IO Bool
isDue (Card _ _ score lastStudied) = do
    now <- getCurrentTime
    return $ diffUTCTime now lastStudied > intervalAt score

-- | Create a new card.
newCard :: String -> String -> IO Card
newCard front back = Card front back 0 <$> getCurrentTime

-- | Create a new two-way card (both front-back and back-front)
newTwoWayCard :: String -> String -> IO (Card, Card)
newTwoWayCard front back = fmap (Card front back 0 &&& Card back front 0) getCurrentTime

-- | Update a card's score, given a feedback. Also reset its last studied time
-- to now.
updateCard :: Feedback -> Card -> IO Card
updateCard feedback card = do
    now <- getCurrentTime
    return $ card &
        (cardScore %~ score feedback) .
        (cardLastStudied .~ now)
  where
    score :: Feedback -> Int -> Int
    score Wrong = pred
    score Hard  = succ
    score Easy  = succ . succ
