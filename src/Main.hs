{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Control.Lens        ((^.))
import Control.Monad       (forever)
import System.IO           (BufferMode(..), hSetBuffering, stdout)
import System.Directory    (createDirectoryIfMissing)

import Config              (kerchiefDir)
import Deck                (deckName)
import Handler             (handleInput)
import Kerchief            (getDeck, runKerchief)
import Utils               (io, prompt)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    kerchiefDir >>= createDirectoryIfMissing False
    printHelp
    runKerchief $ forever $ do
        deckName <- maybe "" (^.deckName) <$> getDeck
        io (prompt $ deckName ++ "> ") >>= handleInput

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Kerchief by MitchellSalad"
    , "+----------------------------------------------+"
    , "| add    - add a card to the current deck      |"
    , "| edit   - edit a card in the current deck     |"
    , "| exit   - exit (will prompt save)             |"
    , "| decks  - show the available decks            |"
    , "| load   - load a deck                         |"
    , "| help   - print this help                     |"
    , "| print  - print the current deck              |"
    , "| rename - rename a deck                       |"
    , "| rm     - remove a card from the current deck |"
    , "| rmdeck - remove a deck                       |"
    , "| save   - save the current deck               |"
    , "| study  - study the current deck              |"
    , "+----------------------------------------------+"
    ]
