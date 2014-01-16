{-# LANGUAGE LambdaCase #-}

module Handler.Study (handleStudy) where

import           Control.Lens   ((^.))

import           Card
import           Deck
import           Kerchief
import           Utils          (io)
import qualified Data.Set.Extra as S

handleStudy :: [String] -> Kerchief ()
handleStudy [] = getDeck >>= maybe (io $ putStrLn "No deck loaded. Try \"load --help\".") handleStudy'
handleStudy _  = io $ putStrLn "Usage: study"

handleStudy' :: Deck -> Kerchief ()
handleStudy' deck = io (S.randomElem (deck^.deckDueCards)) >>= 
    maybe (io $ putStrLn "No cards due!") handleStudyCard

handleStudyCard :: Card -> Kerchief ()
handleStudyCard card@(Card front back _ _) = do
    io $ putStrLn front
    io $ putStr "Press enter to continue, or input \"-\" to go back. "
    io getLine >>= \case
        "-" -> return ()
        _   -> do
            io $ putStrLn back
            promptFeedback
  where
    promptFeedback :: Kerchief ()
    promptFeedback = do
        io $ putStrLn "1 - easy, 2 - hard, 3 - wrong"
        io getLine >>= \case
            "1" -> modifyDeckIO (studyCard Easy card)
            "2" -> modifyDeckIO (studyCard Hard card)
            "3" -> modifyDeckIO (studyCard Wrong card)
            _   -> io (putStrLn "Please enter a valid integer.") >> promptFeedback
