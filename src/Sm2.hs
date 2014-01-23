{-# LANGUAGE ViewPatterns #-}

module Sm2 where

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

-- | A SuperMemoable has a Lens on both an EasinessFactor and an [Interval].
-- The [Interval] is a reverse-order list of days representing the entire
-- history of the
class SuperMemoable a where
    smFactor    :: Lens' a EasinessFactor
    smIntervals :: Lens' a [Interval]

-- | Get the current inter-repetition interval. Querying for this value is
-- undefined until sm2 has been run at least once.
smInterval :: SuperMemoable a => a -> Interval
smInterval m = head (m^.smIntervals)

-- | Initialize a SuperMemoable. This is necessary before running sm2 for the
-- first time.
initializeSuperMemo :: SuperMemoable a => a -> a
initializeSuperMemo = (smFactor .~ 2.5) . (smIntervals .~ [])

-- | Given a Response, update a SuperMemoable. @initializeSuperMemo@ should be
-- called before calling this function for the first time.
--
-- See http://www.supermemo.com/english/ol/sm2.htm for more information.
sm2 :: SuperMemoable a => Response -> a -> a
sm2 (fromEnum -> q) m
    | q < 3 = m & smIntervals .~ []
    | otherwise = m & smIntervals .~ newIntervals
                    & smFactor    .~ newFactor
  where
    intervals = m^.smIntervals
    efactor   = m^.smFactor

    newIntervals :: [Interval]
    newIntervals = case intervals of
        []       -> [1]
        [_]      -> [6,1]
        xs@(x:_) -> (ceiling $ fromIntegral x * efactor) : xs

    newFactor :: EasinessFactor
    newFactor = max 1.3 newFactor'
      where
        newFactor' :: EasinessFactor
        newFactor' = efactor + 0.1 - (5-q')*(0.08 + (5-q')*0.02)

        q' :: Float
        q' = fromIntegral q
