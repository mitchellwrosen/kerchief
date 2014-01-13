{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Monad (void)
import Data.Foldable

-- | Print each element of a Foldable, prepended by a number (starting at 1).
printNumbered :: forall a t. (Show a, Foldable t) => t a -> IO ()
printNumbered = void . foldlM f 1
  where
    f :: Int -> a -> IO Int
    f n card = putStrLn (show n ++ ". " ++ show card) >> return (n+1)
