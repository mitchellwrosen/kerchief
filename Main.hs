{-# LANGUAGE LambdaCase, ScopedTypeVariables, ViewPatterns #-}

module Main where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import System.IO           (hFlush, stdout)

import Deck                (newDeck)
import Handler             (handleInput)
import Kerchief            (runKerchief, useDeck)

main :: IO ()
main = runKerchief $ do
    useDeck (newDeck "test deck")
    liftIO printHelp
    forever $
        liftIO (prompt "[~] $ ") >>= handleInput

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Kerchief by MitchellSalad"
    , "Type \"ls\" to get started. \"exit\" to exit."
    ]

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine
