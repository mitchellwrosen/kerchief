module Handler.Load (handleLoad) where

import Kerchief.Prelude 
import Prelude hiding (putStr, putStrLn)

import           Data.List (intercalate)
import qualified Data.Set  as S

import Deck                (deckCards, deckDueCards, dueRatio, emptyDeck)
import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief            (Kerchief, getDecksDir, loadDeck, setDeck)
import Utils               (askYesNo, getDirectoryContents')

handleLoad :: [String] -> Kerchief ()
handleLoad ["--help"]   = printLoadUsage
handleLoad [name]       = handleLoadName name
handleLoad _            = printLoadUsage

printLoadUsage :: Kerchief ()
printLoadUsage = do
    putStr "Available decks: "
    getDecksDir >>= io . getDirectoryContents' >>= putStrLn . intercalate ", "
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
        setDeck (emptyDeck name)
        putStrLn $ "\"" ++ name ++ "\" created."
