{-# LANGUAGE ViewPatterns #-}

module SuperMemo2 where

import Control.Lens

type EasinessFactor = Float
type Interval = Int

-- | User-supplied response to a prompt.
data Response
    = Response0 -- ^ Complete blackout
    | Response1 -- ^ Incorrect response; correct one remembered
    | Response2 -- ^ Incorrect response; correct one seemed easy to recall
    | Response3 -- ^ Correct response recalled with serious difficulty
    | Response4 -- ^ Correct response after a hesitation
    | Response5 -- ^ Perfect response
    deriving Enum -- don't change the ordering of these, because of this

-- | A SuperMemoable has a Lens on both an EasinessFactor and an Interval.
-- These are implementation details and shouldn't be touched, except by
-- 'superMemo2'. Always call 'initializeSuperMemo' once before running the
-- algorithm.
class SuperMemoable a where
    smFactor   :: Lens' a EasinessFactor
    smInterval :: Lens' a Interval

-- | Initialize a SuperMemoable. This is necessary before running sm2 for the
-- first time.
initializeSuperMemo :: SuperMemoable a => a -> a
initializeSuperMemo = (smFactor .~ 2.5) . (smInterval .~ 0)

-- | Given a Response, update a SuperMemoable. 'initializeSuperMemo' should be
-- called before calling this function for the first time.
--
-- See http://www.supermemo.com/english/ol/sm2.htm for more information.
superMemo2 :: SuperMemoable a => Response -> a -> a
superMemo2 (fromEnum -> q) m
    | q < 3 = m & smInterval .~ 0
    | otherwise = m & smInterval .~ newInterval interval
                    & smFactor   .~ newFactor
  where
    interval = m^.smInterval
    efactor  = m^.smFactor

    newInterval :: Interval -> Interval
    newInterval 0 = 1
    newInterval 1 = 6
    newInterval n = ceiling $ fromIntegral n * efactor

    newFactor :: EasinessFactor
    newFactor = bound 1.3 2.5 newFactor'
      where
        bound :: Ord a => a -> a -> a -> a
        bound theMin theMax = min theMax . max theMin

        newFactor' :: EasinessFactor
        newFactor' = efactor + 0.1 - (5-q')*(0.08 + (5-q')*0.02)

        q' :: Float
        q' = fromIntegral q
