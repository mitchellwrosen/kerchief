module Handler.Load (handleLoad) where

import Control.Lens ((^.))
import Data.List           (intercalate)
import qualified Data.Set as S

import Config              (kerchiefDir)
import Deck                (deckCards, deckDueCards, newDeck)
import Handler.Utils       (promptSaveCurrentDeck)
import Kerchief
import Utils               (askYesNo, getDirectoryContents', io)

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
    loadDeckByName name >>= 
        maybe 
            (askYesNo ("Create deck \"" ++ name ++ "\"? (y/n) ")
                      createAndLoadNewDeck
                      (return ()))
            (\deck -> do
                let numDue = S.size (deck^.deckDueCards)
                let totalCards = S.size (deckCards deck)
                io . putStrLn $ 
                    "\"" ++ name ++ "\" loaded. (" ++ show numDue ++ "/" ++ show totalCards ++ " cards due)")
  where
    createAndLoadNewDeck :: Kerchief ()
    createAndLoadNewDeck = do
        loadDeck (newDeck name)
        io . putStrLn $ "\"" ++ name ++ "\" created."
