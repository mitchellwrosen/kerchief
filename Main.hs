{-# LANGUAGE LambdaCase, ScopedTypeVariables, ViewPatterns #-}

module Main where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import System.IO           (BufferMode(..), hSetBuffering, stdout)
import System.Directory    (createDirectoryIfMissing, getHomeDirectory)

import Handler             (handleInput)
import Kerchief            (runKerchief)
import Utils               (prompt)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    homeDir <- getHomeDirectory
    createDirectoryIfMissing False (homeDir ++ "/.kerchief")

    printHelp
    runKerchief $ forever $
        liftIO (prompt "[~] $ ") >>= handleInput

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Kerchief by MitchellSalad"
    , "Type \"ls\" to get started. \"exit\" to exit."
    ]
