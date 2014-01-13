{-# LANGUAGE LambdaCase #-}

module Handler.Remove (handleRemove) where

import Control.Monad.Trans (liftIO)
import Data.Set (Set)
import qualified Data.Set as S

import Card
import Deck
import Kerchief
import Utils

handleRemove :: [String] -> Kerchief ()
handleRemove ["--help"]  = liftIO printRemoveUsage
handleRemove [word]      = handleRemoveWord word
handleRemove [word,deck] = handleRemoveWordDeck word deck
handleRemove _           = liftIO printRemoveUsage

printRemoveUsage :: IO ()
printRemoveUsage = mapM_ putStrLn
    [ "Usage: remove word [deck]"
    , ""
    , "search for cards containing /word/ from /deck/,"
    , "or the current deck if /deck/ is absent, and"
    , "prompt for their removal"
    ]

handleRemoveWord :: String -> Kerchief ()
handleRemoveWord word = getCurrentDeck >>= maybe noDeck yesDeck
  where
    noDeck :: Kerchief ()
    noDeck = liftIO $ putStrLn "No deck selected. Use \"deck\", or the optional deck arg. See \"remove --help\"."

    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = removeWordDeck word deck

handleRemoveWordDeck :: String -> String -> Kerchief ()
handleRemoveWordDeck word deck = loadDeck deck >>= maybe
    (liftIO . putStrLn $ "Deck \"" ++ deck ++ "\" not found. Try \"ls decks/\" or \"deck --list\".")
    (removeWordDeck word)

removeWordDeck :: String -> Deck -> Kerchief ()
removeWordDeck word deck = do
    let cards = searchDeck word deck
    if S.null cards
        then liftIO $ putStrLn "No cards found."
        else loop cards
  where
    -- "loop" only so long as the user is inputting bad data.
    loop :: Set Card -> Kerchief ()
    loop cards = do
        liftIO $ printNumbered cards
        liftIO $ putStrLn "Remove which definition? (\"-\" to go back)"
        liftIO getLine >>= \case
            "-" -> liftIO $ putStrLn "No cards removed."
            s   -> case reads s of
                [(n,"")] | n >= 0 && n < S.size cards -> putDeck $ removeCard deck (S.elemAt n cards)
                _ -> do
                    liftIO $ putStrLn "Please pick a valid integer."
                    loop cards
