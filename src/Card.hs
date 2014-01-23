{-# LANGUAGE TemplateHaskell #-}

module Card 
    ( 
    -- * Card type
      Card
    , cardBack
    , cardFront
    , cardInterval
    , cardLastStudied
    , Feedback(..)
    -- * Creating cards
    , newCard
    -- * Modifying cards
    , reverseCard
    , setCardContents
    , updateCard
    -- * Querying cards
    , containsText
    , isDue
    , showCard
    -- * TODO
    , nthEntry
    ) where

import Control.Applicative
import Control.Lens
import Data.List                    (isInfixOf)
import Data.Monoid                  ((<>))
import Data.Serialize
import Data.Time.Clock

import Data.Time.Instances          ()
import Network.API.GoogleDictionary (Entry(..))

import Sm2

--
-- Card type
--

data Card = Card
    { _cardFront          :: !String
    , _cardBack           :: !String
    , _cardLastStudied    :: !UTCTime
    -- For SuperMemoable instance
    , _cardEasinessFactor :: !EasinessFactor
    , _cardIntervals      :: [Interval]
    } deriving Show
makeLenses ''Card

-- | User-supplied feedback on the difficulty of a card.
data Feedback = Easy | Hard | Wrong

-- Compare cards on their contents, not score or timestamp.
instance Eq Card where
    (Card f1 b1 _ _ _) == (Card f2 b2 _ _ _) = f1 == f2 && b1 == b2

instance Ord Card where
    compare (Card f1 b1 _ _ _) (Card f2 b2 _ _ _) = compare f1 f2 <> compare b1 b2

instance Serialize Card where
    put (Card a b c d e) = put a >> put b >> put c >> put d >> put e
    get = Card <$> get <*> get <*> get <*> get <*> get

instance SuperMemoable Card where
    smFactor    = cardEasinessFactor
    smIntervals = cardIntervals

--
-- Creating cards
--

-- | Create a new 'Card' with the given front/back content.
newCard :: String -> String -> IO Card
newCard front back = updateTimestamp $ initializeSuperMemo 
    (Card front back undefined undefined undefined) -- undefineds are set immediately

-- Set a card's last-studied-time to now.
updateTimestamp :: Card -> IO Card
updateTimestamp card = (\time -> set cardLastStudied time card) <$> getCurrentTime

--
-- Modifying cards
--

-- | Reverse a 'Card' 's back and front.
reverseCard :: Card -> Card
reverseCard card = card & cardFront .~ (card^.cardBack)
                        & cardBack  .~ (card^.cardFront)

-- | Set a 'Card' 's front/back contents.
setCardContents :: String -> String -> Card -> Card
setCardContents front back card = card & cardFront .~ front & cardBack .~ back

-- | Update a 'Card', given user 'Feedback'. Also updates its last-studied
-- timestamp, hence IO.
updateCard :: Feedback -> Card -> IO Card
updateCard feedback = updateTimestamp . sm2 (feedbackToResponse feedback)
  where
    -- Translate a Feedback (defined here) to a Response (SuperMemo)
    feedbackToResponse :: Feedback -> Response
    feedbackToResponse Easy  = Response5
    feedbackToResponse Hard  = Response3
    feedbackToResponse Wrong = Response0

--
-- Querying cards
--

-- | Calculate the interval after which this 'Card' is due.
cardInterval :: Card -> NominalDiffTime
cardInterval card = realToFrac $ secondsToDiffTime (days * 86400)
  where
    days :: Integer
    days = fromIntegral (smInterval card)

-- | Search a 'Card' 's contents for the given 'String'.
containsText :: String -> Card -> Bool
containsText str (Card front back _ _ _) = isInfixOf str front || isInfixOf str back

-- | Check if this 'Card' is due for studying.
isDue :: Card -> IO Bool
isDue card = do
    now <- getCurrentTime
    return $ diffUTCTime now (card ^. cardLastStudied) > cardInterval card

-- | Alternative Show instance that shows front/back contents.
showCard :: Card -> String
showCard (Card front back _ _ _) = "[Front] " ++ front ++ "\n [Back] " ++ back

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
