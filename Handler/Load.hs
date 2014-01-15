module Handler.Load (handleLoad) where

import Control.Monad.Trans (liftIO)

import Card          (showCard)
import Deck          (deckCards, newDeck)
import Handler.Ls    (handleLs)
import Handler.Utils (promptSaveCurrentDeck)
import Kerchief
import Utils         (askYesNo, printNumberedWith, unless')

handleLoad :: [String] -> Kerchief ()
handleLoad ["--help"]   = liftIO printLoadUsage
handleLoad [name]       = handleLoadName name
handleLoad _            = liftIO printLoadUsage

printLoadUsage :: IO ()
printLoadUsage = mapM_ putStrLn
    [ "Usage: load deck"
    , "load (or create) deck from file"
    ]

handleLoadName :: String -> Kerchief ()
handleLoadName name = loadDeckByName name >>= unless' promptCreateNewDeck
  where
    promptCreateNewDeck :: Kerchief ()
    promptCreateNewDeck = askYesNo ("Create deck \"" ++ name ++ "\"? (y/n) ")
                                   createAndLoadNewDeck
                                   (return ())

    createAndLoadNewDeck :: Kerchief ()
    createAndLoadNewDeck = do
        promptSaveCurrentDeck
        loadDeck (newDeck name)
        liftIO . putStrLn $ "Deck \"" ++ name ++ "\" created."
