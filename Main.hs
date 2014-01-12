{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

import Control.Monad        (forever)
import Control.Monad.Extras (whenMaybe)

import Card                 (nthEntry)
import Deck                 (Deck, addNewCard, newDeck, printDeck)
import Dictionary           (Entry, lookupWord)

main :: IO ()
main = do
    deck <- newDeck "test"
    loop deck
  where
    loop :: Deck -> IO ()
    loop deck = forever $ do
        putStrLn "1 - add card, 2 - print cards"
        getLine >>= \case
            "1" -> doAddCard deck
            "2" -> printDeck deck
            _   -> return ()

doAddCard :: Deck -> IO ()
doAddCard deck = do
    putStrLn "What word?"
    getLine >>= lookupWord >>= whenMaybe (\entry -> print entry >> pickDefinition entry)
  where
    pickDefinition :: Entry -> IO ()
    pickDefinition entry = do
        putStrLn "Which definition? (0 = none)"
        fmap reads getLine >>= pickDefinition' entry

    pickDefinition' :: Entry -> [(Int,String)] -> IO ()
    pickDefinition' _     [(0,"")] = print "No card added."
    pickDefinition' entry [(n,"")] = maybe (pickDefinition entry) doAddCard' (nthEntry n entry)
    pickDefinition' entry _        = print "Please pick a valid integer." >> pickDefinition entry

    doAddCard' :: (String, String) -> IO ()
    doAddCard' (front,back) = addNewCard front back deck >> print "Card added."
