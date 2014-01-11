module Main where

import System.Environment (getArgs)

import Dictionary

main :: IO ()
main = getArgs >>= mapM_ (\w -> lookupWord w >>= whenMaybe print)

whenMaybe :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenMaybe = maybe (return ())
