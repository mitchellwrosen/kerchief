{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Control.Monad.Reader (asks)
import System.IO            (BufferMode(..), hSetBuffering, stdout)
import System.Directory     (createDirectoryIfMissing)

import Deck                 (deckName)
import Handler              (handleInput)
import Kerchief             (Kerchief, getDecksDir, getKerchiefDir, getSoundbytesDir, getDeck, runKerchief)
import Utils                (prompt)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    printHelp
    runKerchief $ do
        makeDirectories
        forever $ do
            deckName <- maybe "" (^.deckName) <$> getDeck
            io (prompt $ deckName ++ "> ") >>= handleInput

makeDirectories :: Kerchief ()
makeDirectories = do
    kerchiefDir   <- getKerchiefDir
    decksDir      <- getDecksDir
    soundbytesDir <- getSoundbytesDir
    mapM_ (io . createDirectoryIfMissing False) [kerchiefDir, decksDir, soundbytesDir]

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Kerchief by MitchellSalad"
    , "+-----------------------------------------------+"
    , "| add     - add a card to the current deck      |"
    , "| addf    - add cards from a file ('batch add') |"
    , "| edit    - edit a card in the current deck     |"
    , "| exit    - exit (will prompt save)             |"
    , "| decks   - show the available decks            |"
    , "| load    - load a deck                         |"
    , "| help    - print this help                     |"
    , "| print   - print the current deck              |"
    , "| rename  - rename a deck                       |"
    , "| rm      - remove a card from the current deck |"
    , "| rmdeck  - remove a deck                       |"
    , "| save    - save the current deck               |"
    , "| study   - study the current deck              |"
    , "+-----------------------------------------------+"
    ]
