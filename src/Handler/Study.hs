{-# LANGUAGE LambdaCase #-}

module Handler.Study (handleStudy) where

import Kerchief.Prelude

import Data.Time.Clock
import Data.List                (intercalate)

import           Card
import           Deck
import           Handler.Utils  (printNoDeckLoadedError)
import           Kerchief
import qualified Data.Set.Extra as S

import Prelude hiding (getLine, putStr, putStrLn)

handleStudy :: [String] -> Kerchief ()
handleStudy [] = getDeck >>= \case
    Nothing -> printNoDeckLoadedError
    Just deck -> handleStudy' True deck >>= setDeck >> putStrLn "No cards due!"
handleStudy _  = putStrLn "Usage: study"

-- True if the deck should be updated when dueCards runs out,
-- False if the deck has already been updated and thus there really are no
-- due cards.
handleStudy' :: Bool -> Deck -> Kerchief Deck
handleStudy' shouldUpdate deck =
    io (S.randomElem (deck^.deckDueCards)) >>=
        maybe (if shouldUpdate 
                   then io (updateDeck deck) >>= handleStudy' False
                   else return deck)
              handleStudyCard
  where
    handleStudyCard :: Card -> Kerchief Deck
    handleStudyCard card = do
        putStrLn (card^.cardFront)
        putStr "Enter to continue, \"p\" to play soundbyte, \"-\" to go back. "
        getLine >>= \case
            "p" -> maybe (putStrLn "No soundbyte available." >> handleStudyCard card)
                         (\url -> io (playSoundUrl url) >> handleStudyCard card)
                         (card^.cardFrontSoundUrl)
            "-" -> return deck
            _   -> do
                putStrLn (card^.cardBack)
                io promptFeedback >>= handleStudy' True -- Keep studying until there are no cards due.
      where
        promptFeedback :: IO Deck
        promptFeedback = do
            cardEasy  <- updateCard Easy card
            cardHard  <- updateCard Hard card
            cardWrong <- updateCard Wrong card
            let cardEasyInterval  = cardInterval cardEasy
            let cardHardInterval  = cardInterval cardHard
            let cardWrongInterval = cardInterval cardWrong

            putStrLn $ "1 - easy ("  ++ prettyPrintDiffTime cardEasyInterval  ++ "), " ++
                       "2 - hard ("  ++ prettyPrintDiffTime cardHardInterval  ++ "), " ++
                       "3 - wrong (" ++ prettyPrintDiffTime cardWrongInterval ++ ")"

            promptFeedback' cardEasy cardHard cardWrong
          where
            promptFeedback' :: Card -> Card -> Card -> IO Deck
            promptFeedback' easy hard wrong =
                getLine >>= \case
                    "1" -> return $ studyCard' easy deck
                    "2" -> return $ studyCard' hard deck
                    "3" -> return $ studyCard' wrong deck
                    _   -> putStrLn "Please input 1, 2, or 3." >> promptFeedback' easy hard wrong

playSoundUrl :: String -> IO ()
playSoundUrl _ = return ()

prettyPrintDiffTime :: NominalDiffTime -> String
prettyPrintDiffTime = inner . ceiling
  where
    inner :: Integer -> String
    inner n = case inner' of
        [] -> "now"
        xs -> intercalate ", " . map disp . take 2 $ xs -- only show 2 units of accuracy
      where
        inner' :: [(Integer, String)]
        inner' = filter (\(num,_) -> num > 0) -- Don't show empty amounts (e.g. drop "0 months")
                     [(months,"months"), (days,"days"), (hours,"hours"), (minutes,"minutes")]

        (months,  o) = n `divMod` 2592000
        (days,    p) = o `divMod` 86400
        (hours,   q) = p `divMod` 3600
        minutes      = q `div`    60

        -- Show the (integer,units) pair, and account for 1 being not plural.
        disp :: (Integer, String) -> String
        disp (1,str) = "1 " ++ init str
        disp (m,str) = show m ++ " " ++ str
