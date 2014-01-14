{-# LANGUAGE LambdaCase #-}

module Handler.Add (handleAdd) where

import Data.Dictionary.Google (Entry, lookupWord)
import Control.Monad.Trans

import Card                   (nthEntry)
import Deck
import Kerchief

handleAdd :: [String] -> Kerchief ()
handleAdd ["--help"]  = liftIO printAddUsage
handleAdd [word]      = handleAddWord word
handleAdd [word,deck] = handleAddWordDeck word deck
handleAdd _           = liftIO printAddUsage

printAddUsage :: IO ()
printAddUsage = mapM_ putStrLn
    [ "Usage: add word [deck]"
    , ""
    , "add /word/ to /deck/, or the current deck if"
    , "/deck/ is absent"
    ]

handleAddWord :: String -> Kerchief ()
handleAddWord word = do
    loaded <- isDeckLoaded
    if loaded
        then addWord word
        else liftIO $ putStrLn "No deck selected. Use \"deck\", or the optional deck arg. See \"add --help\"."

handleAddWordDeck :: String -> String -> Kerchief ()
handleAddWordDeck word deck = do
    loaded <- loadDeck deck
    if loaded
            then addWord word
            else liftIO . putStrLn $ "Deck \"" ++ deck ++ "\" not found. Try \"ls decks/\" or \"deck --list\"."

-- TODO - offer to add the reverse card
addWord :: String -> Kerchief ()
addWord word = liftIO (lookupWord word) >>=
    maybe (liftIO $ putStrLn "No definition found.")
          (\entry -> liftIO (putStrLn "" >> print entry) >> pickDefinition entry)
  where
    pickDefinition :: Entry -> Kerchief ()
    pickDefinition entry = do
        liftIO $ putStrLn "Which definition? (\"-\" to go back)"
        liftIO getLine >>= \case
            "-" -> liftIO $ putStrLn "No card added."
            s   -> pickDefinition' entry (reads s)

    pickDefinition' :: Entry -> [(Int,String)] -> Kerchief ()
    pickDefinition' entry [(n,"")] = maybe (bad entry) doAddCard (nthEntry (n-1) entry)
    pickDefinition' entry _        = bad entry

    bad :: Entry -> Kerchief ()
    bad entry = liftIO (putStrLn "Please pick a valid integer.") >> pickDefinition entry

    doAddCard :: (String, String) -> Kerchief ()
    doAddCard (front,back) = do
        modifyDeckIO $ addNewCard front back
        liftIO $ putStrLn "Card added."
