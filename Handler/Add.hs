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
    , "adds word to deck, or the current deck if absent"
    ]

handleAddWord :: String -> Kerchief ()
handleAddWord word = getDeck >>= maybe noDeck yesDeck
  where
    noDeck :: Kerchief ()
    noDeck = liftIO $ putStrLn "No deck selected. Use \"deck\", or the optional deck arg. See \"add --help\"."

    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = liftIO $ addWordDeck word deck

handleAddWordDeck :: String -> String -> Kerchief ()
handleAddWordDeck word deck = liftIO (putStrLn "TODO")

addWordDeck :: String -> Deck -> IO ()
addWordDeck word deck = lookupWord word >>=
    maybe (putStrLn "No definition found.")
          (\entry -> putStrLn "" >> print entry >> pickDefinition entry)
  where
    pickDefinition :: Entry -> IO ()
    pickDefinition entry = do
        putStrLn "Which definition? (\"-\" to go back)"
        getLine >>= \case
            "-" -> putStrLn "No card added."
            s   -> pickDefinition' entry (reads s)

    pickDefinition' :: Entry -> [(Int,String)] -> IO ()
    pickDefinition' entry [(n,"")] = maybe (bad entry) doAddCard' (nthEntry (n-1) entry)
    pickDefinition' entry _        = bad entry

    bad :: Entry -> IO ()
    bad entry = putStrLn "Please pick a valid integer." >> pickDefinition entry

    doAddCard' :: (String, String) -> IO ()
    doAddCard' (front,back) = addNewCard front back deck >> putStrLn "Card added."
