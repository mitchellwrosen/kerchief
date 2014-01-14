{-# LANGUAGE LambdaCase, ScopedTypeVariables, ViewPatterns #-}

module Main where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import System.IO           (BufferMode(..), hSetBuffering, stdout)

import Handler             (handleInput)
import Kerchief            (loadDeck, runKerchief)
import Utils               (prompt)

main :: IO ()
main = runKerchief $ do
    liftIO $ hSetBuffering stdout NoBuffering

    _ <- loadDeck "testDeck"
    liftIO printHelp
    forever $
        liftIO (prompt "[~] $ ") >>= handleInput

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Kerchief by MitchellSalad"
    , "Type \"ls\" to get started. \"exit\" to exit."
    ]
