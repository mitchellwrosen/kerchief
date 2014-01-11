module Richards where

intervalAt :: Int -> Double
intervalAt t = richards a k b v q m t
  where
    -- Lower asymtote
    a = 1
    -- Upper asymtote
    k = 262974 -- 6 months in minutes
    -- Growth rate
    b = 1
    -- Affects near which asymtote maximum growth occurs
    v = 1
    -- Depends on the value richards(0)
    q = 1
    -- The time of maximum growth if Q=v
    m = 12

-- | http://en.wikipedia.org/wiki/Generalised_logistic_function
richards :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> Double
richards a k b v q m t = a + num/denom
  where
    num = k - a
    denom = (1 + q*exp (-b*(fromIntegral t - m))) ** (1/v)
