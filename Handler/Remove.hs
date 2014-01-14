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
handleRemoveWord word = do
    loaded <- isDeckLoaded
    if loaded
        then removeWord word
        else liftIO $ putStrLn "No deck loaded. Use \"deck\", or the optional deck arg. See \"remove --help\"."

handleRemoveWordDeck :: String -> String -> Kerchief ()
handleRemoveWordDeck word deck = do
    loaded <- loadDeck deck
    if loaded
        then removeWord word
        else liftIO . putStrLn $ "Deck \"" ++ deck ++ "\" not found. Try \"ls decks/\" or \"deck --list\"."

removeWord :: String -> Kerchief ()
removeWord word = do
    Just deck <- getDeck -- TODO: Partial function :(
    let cards = searchDeck word deck
    if S.null cards
        then liftIO $ putStrLn "No matching cards found."
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
                [(n,"")] | n >= 0 && n < S.size cards -> modifyDeck (removeCard $ S.elemAt n cards)
                _ -> do
                    liftIO $ putStrLn "Please pick a valid integer."
                    loop cards
