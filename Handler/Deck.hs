module Handler.Deck (handleDeck) where

import Control.Monad.Trans (liftIO)

import Card          (showCard)
import Deck          (deckCards, newDeck)
import Handler.Ls    (handleLs)
import Handler.Utils (promptSaveCurrentDeck)
import Kerchief
import Utils         (askYesNo, printNumberedWith, unless')

handleDeck :: [String] -> Kerchief ()
handleDeck ["--help"]   = liftIO printDeckUsage
handleDeck ["--print"]  = handleDeckPrint
handleDeck ["--list"]   = liftIO $ handleLs ["decks"] -- "deck --list" is a synonym for "ls decks"
handleDeck [name]       = handleDeckName name
handleDeck _            = liftIO printDeckUsage

printDeckUsage :: IO ()
printDeckUsage = mapM_ putStrLn
    [ "Usage: deck [name|--print|--list]"
    , ""
    , "deck name: load (or create) deck |name| and set it to be the current deck"
    , "deck --print: print the cards in the current deck"
    , "deck --list: list all decks"
    , ""
    ]

handleDeckPrint :: Kerchief ()
handleDeckPrint = getDeck >>= liftIO . maybe (putStrLn "No deck selected.")
                                             (printNumberedWith showCard . deckCards)

handleDeckName :: String -> Kerchief ()
handleDeckName name = loadDeckByName name >>= unless' promptCreateNewDeck
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
