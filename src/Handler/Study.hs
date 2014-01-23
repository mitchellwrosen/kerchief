{-# LANGUAGE LambdaCase #-}

module Handler.Study (handleStudy) where

import Control.Lens
import Data.Time.Clock
import Data.List                (intercalate)

import           Card
import           Deck
import           Kerchief
import           Utils          (io)
import qualified Data.Set.Extra as S

handleStudy :: [String] -> Kerchief ()
handleStudy [] = getDeck >>= maybe (io $ putStrLn "No deck loaded. Try \"load --help\".") handleStudy'
handleStudy _  = io $ putStrLn "Usage: study"

handleStudy' :: Deck -> Kerchief ()
handleStudy' (Deck _ dueCards _) =
    io (S.randomElem (dueCards)) >>=
        maybe (io $ putStrLn "No cards due!")
              handleStudyCard

handleStudyCard :: Card -> Kerchief ()
handleStudyCard card = do
    io $ putStrLn (card^.cardFront)
    io $ putStr "Press enter to continue, or input \"-\" to go back. "
    io getLine >>= \case
        "-" -> return ()
        _   -> do
            io $ putStrLn (card^.cardBack)
            promptFeedback
            handleStudy [] -- Keep studying until there are no cards due.
  where
    promptFeedback :: Kerchief ()
    promptFeedback = do
        cardEasy  <- io $ updateCard Easy card
        cardHard  <- io $ updateCard Hard card
        cardWrong <- io $ updateCard Wrong card
        let cardEasyInterval  = cardInterval cardEasy
        let cardHardInterval  = cardInterval cardHard
        let cardWrongInterval = cardInterval cardWrong

        io . putStrLn $ "1 - easy ("  ++ prettyPrintDiffTime cardEasyInterval  ++ "), " ++
                        "2 - hard ("  ++ prettyPrintDiffTime cardHardInterval  ++ "), " ++
                        "3 - wrong (" ++ prettyPrintDiffTime cardWrongInterval ++ ")"

        loop cardEasy cardHard cardWrong
      where
        loop :: Card -> Card -> Card -> Kerchief ()
        loop easy hard wrong =
            io getLine >>= \case
                "1" -> modifyDeck (studyCard' easy)
                "2" -> modifyDeck (studyCard' hard)
                "3" -> modifyDeck (studyCard' wrong)
                _   -> io (putStrLn "Please input 1, 2, or 3.") >> loop easy hard wrong

prettyPrintDiffTime :: NominalDiffTime -> String
prettyPrintDiffTime = inner . ceiling
  where
    inner :: Integer -> String
    inner n = [(months,"months"), (days,"days"), (hours,"hours"), (minutes,"minutes")]
        & filter (\(num,_) -> num > 0) -- Don't show empty amounts (e.g. drop "0 months")
        & take 2                       -- Only show two units of accuracy
        & map disp
        & intercalate ", "
      where
        (months,  o) = n `divMod` 2592000
        (days,    p) = o `divMod` 86400
        (hours,   q) = p `divMod` 3600
        minutes      = q `div`    60

        -- Show the (integer,units) pair, and account for 1 being not plural.
        disp :: (Integer, String) -> String
        disp (1,str) = "1 " ++ init str
        disp (m,str) = show m ++ " " ++ str
