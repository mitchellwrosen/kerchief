{-# LANGUAGE LambdaCase #-}

module Handler.Study (handleStudy) where

import Kerchief.Prelude
import Prelude hiding (getLine, putStr, putStrLn)

import Data.Time.Clock
import Data.List                    (intercalate)

import           Card
import qualified Data.Set.Extra as S
import           Deck
import           Handler.Utils      (printNoDeckLoadedError)
import           Kerchief
import           Mp3                (playMp3Url)
import           Network.HTTP.Extra (getResponseBody')
import           Utils              (prompt)

handleStudy :: [String] -> Kerchief ()
handleStudy [] = getDeck >>= \case
    Nothing -> printNoDeckLoadedError
    Just deck -> handleStudy' True deck >>= setDeck
handleStudy _  = putStrLn "Usage: study"

-- True if the deck should be updated when dueCards runs out,
-- False if the deck has already been updated and thus there really are no
-- due cards.
handleStudy' :: Bool -> Deck -> Kerchief Deck
handleStudy' shouldUpdate deck =
    io (S.randomElem (deck^.deckDueCards)) >>=
        maybe (if shouldUpdate 
                   then io (updateDeck deck) >>= handleStudy' False
                   else putStrLn "No cards due!" >> return deck)
              handleStudyCard
  where
    handleStudyCard :: Card -> Kerchief Deck
    handleStudyCard card = do
        putStrLn (card^.cardFront)
        loop1
      where
        loop1 = do
            putStrLn "[f]lip | [p]lay soundbyte | [b]ack"
            loop2

        -- Loop here, entertaining as many "p"s as they want, but re-print the
        -- options (from loop1) if they input a bad character
        loop2 = prompt "> " >>= \case
            "p" -> io (playCard card) >> loop2
            "b" -> return deck
            "f" -> do
                putStrLn (card^.cardBack)
                io promptFeedback >>= handleStudy' True -- Keep studying until there are no cards due.
            _ -> loop1

        promptFeedback :: IO Deck
        promptFeedback = do
            easy  <- updateCard Easy card
            hard  <- updateCard Hard card
            wrong <- updateCard Wrong card
            let easyInterval  = cardInterval easy
            let hardInterval  = cardInterval hard
            let wrongInterval = cardInterval wrong

            promptFeedback' easy hard wrong easyInterval hardInterval wrongInterval

        promptFeedback' :: Card -> Card -> Card -> NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> IO Deck
        promptFeedback' easy hard wrong easyInterval hardInterval wrongInterval = loop1
          where
            loop1 = do
                putStrLn $ "[e]asy ("  ++ prettyPrintDiffTime easyInterval  ++ ") | " ++
                           "[h]ard ("  ++ prettyPrintDiffTime hardInterval  ++ ") | " ++
                           "[w]rong (" ++ prettyPrintDiffTime wrongInterval ++ ") | " ++
                           "[p]lay soundbyte"
                loop2

            loop2 = prompt "> " >>= \case
                "e" -> return $ studyCard' easy deck
                "h" -> return $ studyCard' hard deck
                "w" -> return $ studyCard' wrong deck
                "p" -> playCard card >> loop2
                _   -> loop1

playCard :: Card -> IO ()
playCard card = maybe (putStrLn "No soundbyte available.")
                      (io . playMp3Url)
                      (card^.cardSoundUrl)

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
