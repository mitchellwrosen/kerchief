{-# LANGUAGE LambdaCase #-}

module Handler.Add (handleAdd) where

import Data.Dictionary.Google (Entry, lookupWord)
import Control.Monad.Trans

import Card                   (Card, newCard, nthEntry, reverseCard)
import Deck
import Kerchief
import Utils                  (askYesNo)

-- TODO: "add front back" for making a new card, not from the dictionary
handleAdd :: [String] -> Kerchief ()
handleAdd ["--help"]  = liftIO printAddUsage
handleAdd [word]      = handleAddWord word
handleAdd _           = liftIO printAddUsage

printAddUsage :: IO ()
printAddUsage = mapM_ putStrLn
    [ "Usage: add word"
    , "look up |word|, pick a definition, and add it to the current deck"
    ]

handleAddWord :: String -> Kerchief ()
handleAddWord word = do
    loaded <- isDeckLoaded
    if loaded
        then liftIO (lookupWord word) >>=
            maybe (liftIO . putStrLn $ "No definition found for \"" ++ word ++ "\".")
                  (\entry -> liftIO (putStrLn "" >> print entry) >> pickDefinition entry)
        else liftIO $ putStrLn "No deck loaded. See \"load --help\"."
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
        card <- liftIO $ newCard front back
        doAddCard' card
        askYesNo "Add reverse card as well? (y/n) "
                 (doAddCard' $ reverseCard card)
                 (return ())
      where
          doAddCard' :: Card -> Kerchief ()
          doAddCard' card = do
              modifyDeck $ addCard card
              liftIO $ putStrLn "Card added."
