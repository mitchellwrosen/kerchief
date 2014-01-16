-- | Carefully tuned sigmoid function
-- Currently unused.
module Richards where

import Data.Time.Clock

-- | Given a score, get the interval in minutes.
intervalAt :: Int -> NominalDiffTime
intervalAt = realToFrac . secondsToDiffTime . floor . (*60) . richards a k b v q m
  where
    a = 1      -- Lower asymtote
    k = 262974 -- Upper asymtote                                    -- 6 months in minutes
    b = 1      -- Growth rate
    v = 1      -- Affects near which asymtote maximum growth occurs
    q = 1      -- Depends on the value richards(0)
    m = 12     -- The time of maximum growth if Q=v

-- | http://en.wikipedia.org/wiki/Generalised_logistic_function
richards :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> Double
richards a k b v q m t = a + num/denom
  where
    num = k - a
    denom = (1 + q*exp (-b*(fromIntegral t - m))) ** (1/v)
