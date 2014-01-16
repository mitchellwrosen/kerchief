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
