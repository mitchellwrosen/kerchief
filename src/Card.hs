{-# LANGUAGE TemplateHaskell #-}

module Card where

import Control.Arrow          ((&&&))
import Control.Applicative
import Control.Lens
import Data.List              (isInfixOf)
import Data.Monoid            ((<>))
import Data.Serialize
import Data.Time.Clock

import Data.Time.Instances    ()
import Richards
import Network.API.GoogleDictionary (Entry(..))

-- | User-supplied feedback for the difficulty of a card.
data Feedback = Easy | Hard | Wrong

data Card = Card
    { _cardFront       :: !String
    , _cardBack        :: !String
    , _cardScore       :: !Int
    , _cardLastStudied :: !UTCTime
    } deriving Show
makeLenses ''Card

-- | Compare cards on their contents, not score or timestamp.
instance Eq Card where
    (Card f1 b1 _ _) == (Card f2 b2 _ _) = f1 == f2 && b1 == b2

instance Ord Card where
    compare (Card f1 b1 _ _) (Card f2 b2 _ _) = compare f1 f2 <> compare b1 b2

instance Serialize Card where
    put (Card front back score lastStudied) = put front >> put back >> put score >> put lastStudied
    get = Card <$> get <*> get <*> get <*> get

-- | Show instance for pretty-printing to console.
showCard :: Card -> String
showCard (Card front back _ _) = "[Front] " ++ front ++ " [Back] " ++ back

-- | Check if a card is due for studying.
isDue :: Card -> IO Bool
isDue card = do
    now <- getCurrentTime
    return $ diffUTCTime now (card ^. cardLastStudied) > cardInterval card

-- | Calculate the interval after which a card is due.
cardInterval :: Card -> NominalDiffTime
cardInterval (Card _ _ score _) = intervalAt score

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
    return $ card
        & cardScore %~ score feedback
        & cardLastStudied .~ now
  where
    score :: Feedback -> Int -> Int
    score Wrong = const 0 -- wrong guess resets score to zero
    score Hard  = succ
    score Easy  = succ . succ

-- | Create front/back text from the nth entry. Return Nothing if the index
-- is out of bounds.
nthEntry :: Int -> Entry -> Maybe (String, String)
nthEntry n entry
    | n < 0 || n >= length ds = Nothing
    | otherwise               = Just (front, back)
  where
    ds         = entryData entry
    word       = entryWord entry
    front      = word ++ " (" ++ pos ++ ")"
    (pos,back) = ds !! n

-- | Get this reverse of the given card (at least, "reverse" as far as the Eq
-- instance is concerned).
reverseCard :: Card -> Card
reverseCard (Card front back x y) = Card back front x y

containsText :: String -> Card -> Bool
containsText str (Card front back _ _) = isInfixOf str front || isInfixOf str back
