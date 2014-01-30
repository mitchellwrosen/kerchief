module Handler.Load (handleLoad) where

import Kerchief.Prelude 
import Prelude hiding (putStr, putStrLn)

import           Data.List (intercalate)
import qualified Data.Set  as S

import Config              (kerchiefDir)
import Deck                (deckCards, deckDueCards, dueRatio)
import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief            (Kerchief, loadDeck, newDeck, setDeck)
import Utils               (askYesNo, getDirectoryContents')

handleLoad :: [String] -> Kerchief ()
handleLoad ["--help"]   = io printLoadUsage
handleLoad [name]       = handleLoadName name
handleLoad _            = io printLoadUsage

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
    loadDeck name >>= 
        maybe 
            (askYesNo ("Create deck \"" ++ name ++ "\"? (y/n) ")
                      createDeck
                      (return ()))
            (\deck -> do
                let (numDue, totalCards) = dueRatio deck
                putStrLn $ 
                    "\"" ++ name ++ "\" loaded. (" ++ show numDue ++ "/" ++ show totalCards ++ " cards due)")
  where
    createDeck :: Kerchief ()
    createDeck = do
        newDeck name
        putStrLn $ "\"" ++ name ++ "\" created."
