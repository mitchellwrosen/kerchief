{-# LANGUAGE LambdaCase #-}

module Handler.Remove (handleRemove) where

import           Control.Monad.Trans (liftIO)
import           Data.Foldable       (mapM_)
import           Data.Set            (Set)
import qualified Data.Set            as S

import           Card                (Card, showCard)
import           Deck                (Deck, deckCards, removeCard, searchDeck)
import           Kerchief            (Kerchief, getDeck, modifyDeck)
import           Utils               (elemAt', printNumberedWith, reads')

import Prelude hiding (mapM_)

handleRemove :: [String] -> Kerchief ()
handleRemove ["--help"]  = liftIO printRemoveUsage
handleRemove [word]      = maybe (handleRemoveWord word) handleRemoveIndex (reads' word)
handleRemove _           = liftIO printRemoveUsage

printRemoveUsage :: IO ()
printRemoveUsage = mapM_ putStrLn
    [ "Usage: remove word"
    , "search for cards containing |word| from the current deck, and prompt for"
    , "prompt for their removal"
    ]

handleRemoveWord :: String -> Kerchief ()
handleRemoveWord word = getDeck >>= maybe noDeck yesDeck
  where
    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = do
        let cards = searchDeck word deck
        if S.null cards
            then liftIO $ putStrLn "No matching cards found."
            else loop cards
      where
        -- "loop" only so long as the user is inputting bad data.
        loop :: Set Card -> Kerchief ()
        loop cards = do
            liftIO $ putStrLn "Matching cards found:"
            liftIO $ printNumberedWith showCard cards
            liftIO $ putStr "Remove which card? (\"-\" to go back, \"all\" to remove all) "
            liftIO getLine >>= \case
                "-"   -> liftIO $ putStrLn "No cards removed."
                "all" -> removeAll cards
                s     -> maybe (liftIO (putStrLn "Please pick a valid integer.") >> loop cards)
                               doRemoveCard
                               (reads' s >>= \n -> elemAt' (n-1) cards)

-- index supplied is 1-based
handleRemoveIndex :: Int -> Kerchief ()
handleRemoveIndex n = getDeck >>= maybe noDeck yesDeck
  where
    yesDeck :: Deck -> Kerchief ()
    yesDeck deck = maybe (liftIO $ putStrLn "Please pick a valid integer.")
                         doRemoveCard
                         (elemAt' (n-1) $ deckCards deck)

noDeck :: Kerchief ()
noDeck = liftIO $ putStrLn "No deck loaded. Try \"deck --help\"."

doRemoveCard :: Card -> Kerchief ()
doRemoveCard card = do
    modifyDeck $ removeCard card
    liftIO $ putStrLn "Card removed."

removeAll :: Set Card -> Kerchief ()
removeAll = mapM_ doRemoveCard
