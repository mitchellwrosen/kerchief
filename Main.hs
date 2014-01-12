module Main where

import System.Environment (getArgs)

import Dictionary

main :: IO ()
main = getArgs >>= mapM_ (\w -> lookupWord w >>= whenMaybe print)
