module Handler.Load (handleLoad) where

import Control.Monad.Trans (liftIO)
import Data.List           (intercalate)

import Config              (kerchiefDir)
import Deck                (newDeck)
import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief
import Utils               (askYesNo, getDirectoryContents')

handleLoad :: [String] -> Kerchief ()
handleLoad ["--help"]   = liftIO printLoadUsage
handleLoad [name]       = handleLoadName name
handleLoad _            = liftIO printLoadUsage

printLoadUsage :: IO ()
printLoadUsage = do
    putStr "Available decks: "
    kerchiefDir >>= getDirectoryContents' >>= putStrLn . intercalate ", "
    mapM_ putStrLn
        [ "Usage: load deck"
        , "load (or create) deck from file"
        ]

handleLoadName :: String -> Kerchief ()
handleLoadName name = do
    promptSaveCurrentDeck
    loaded <- loadDeckByName name
    if loaded
        then liftIO . putStrLn $ "\"" ++ name ++ "\" loaded."
        else askYesNo ("Create deck \"" ++ name ++ "\"? (y/n) ")
                      createAndLoadNewDeck
                      (return ())
  where
    createAndLoadNewDeck :: Kerchief ()
    createAndLoadNewDeck = do
        loadDeck (newDeck name)
        liftIO . putStrLn $ "\"" ++ name ++ "\" created."
