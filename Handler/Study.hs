{-# LANGUAGE LambdaCase #-}

module Handler.Study (handleStudy) where

import           Control.Lens        ((^.))
import           Control.Monad.Trans (liftIO)

import Card
import Deck
import Kerchief
import Utils                         (randomElem)

handleStudy :: [String] -> Kerchief ()
handleStudy [] = getDeck >>= maybe (liftIO $ putStrLn "No deck loaded. Try \"deck --help\".") handleStudy'
handleStudy _  = liftIO $ putStrLn "Usage: study"

handleStudy' :: Deck -> Kerchief ()
handleStudy' deck = liftIO (randomElem (deck^.deckDueCards)) >>= 
    maybe (liftIO $ putStrLn "No cards due!") handleStudyCard

handleStudyCard :: Card -> Kerchief ()
handleStudyCard card@(Card front back _ _) = do
    liftIO $ putStrLn front
    liftIO $ putStr "Press enter to continue, or input \"-\" to go back."
    promptFeedback
  where
    promptFeedback :: Kerchief ()
    promptFeedback = do
        liftIO getLine >>= \case
            "-" -> return ()
            _   -> do
                liftIO $ putStrLn back
                liftIO $ putStrLn "1 - easy, 2 - hard, 3 - wrong"
                liftIO getLine >>= \case
                    "1" -> modifyDeckIO (studyCard Easy card)
                    "2" -> modifyDeckIO (studyCard Hard card)
                    "3" -> modifyDeckIO (studyCard Wrong card)
                    _   -> liftIO (putStrLn "Please enter a valid integer.") >> promptFeedback
