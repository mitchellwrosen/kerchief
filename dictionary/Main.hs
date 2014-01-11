{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forever)

import Dictionary

main :: IO ()
main = forever $ do
    putStrLn "Look up what word?"
    getLine >>= lookupWord >>= whenMaybe print

whenMaybe :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenMaybe = maybe (return ())
