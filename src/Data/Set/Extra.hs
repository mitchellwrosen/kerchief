module Data.Set.Extra where

import Data.Set (Set)
import qualified Data.Set as S
import System.Random (randomRIO)

-- | Safe replacement for Data.Set's partial elemAt
safeElemAt :: Int -> Set a -> Maybe a
safeElemAt n s
    | n < 0 || n >= S.size s = Nothing
    | otherwise              = Just (S.elemAt n s)

randomElem :: Set a -> IO (Maybe a)
randomElem s = case S.size s of
    0 -> return Nothing
    n -> fmap (Just . flip S.elemAt s) (randomRIO (0, n-1))

-- | Modify an element in a set by a function. Simply performs a delete followed
-- by an insert, so if the element doesn't exist in the set initially, the set
-- will still be modified.
modify :: Ord a => a -> (a -> a) -> Set a -> Set a
modify x f = S.insert (f x) . S.delete x
