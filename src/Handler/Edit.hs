module Handler.Edit (handleEdit) where

import Kerchief.Prelude

import           Control.Applicative ((<$))
import           Data.Foldable       (foldlM)
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Set.Extra      as S

import           Card                (Card, setCardContents, showCard)
import           Deck                (Deck, deckCards, modifyCard, searchDeck)
import           Handler.Utils       (printNoDeckLoadedError)
import           Kerchief            (Kerchief, getDeck, setDeck)
import           Utils               (askYesNo, printNumberedWith, prompt, reads')

import Prelude hiding (putStr, putStrLn, getLine)

handleEdit :: [String] -> Kerchief ()
handleEdit ["--help"]  = io printEditUsage
handleEdit [word]      = maybe (handleEditWord word) handleEditIndex (reads' word)
handleEdit _           = io printEditUsage

printEditUsage :: IO ()
printEditUsage = mapM_ putStrLn
    [ "Usage: edit [word|index]"
    , "edit word: edit a card containing |word| from the current deck"
    , "edit index: edit card at |index| from the current deck (see \"print\")"
    ]

handleEditWord :: String -> Kerchief ()
handleEditWord word = getDeck >>= maybe printNoDeckLoadedError handleEditWord'
  where
    handleEditWord' :: Deck -> Kerchief ()
    handleEditWord' deck = do
        let cards = searchDeck word deck
        if S.null cards
            then putStrLn "No matching cards found."
            else promptEditCards cards >>= setDeck . ($ deck)

-- loop only so long as the user is inputting bad data.
promptEditCards :: Set Card -> Kerchief (Deck -> Deck)
promptEditCards cards = do
    putStrLn "Matching cards found:"
    printNumberedWith showCard cards
    putStr "Edit which card? (\"-\" to go back, \"all\" to edit all) "
    getLine >>= \ms -> case ms of
        "-"   -> putStrLn "No cards edited." >> return id
        "all" -> foldlM (\f card -> (f .) <$> promptEditCard card) id cards
        s     -> maybe (putStrLn "Please pick a valid integer." >> promptEditCards cards)
                       promptEditCard
                       (reads' s >>= \n -> S.safeElemAt (n-1) cards)

promptEditCard :: Card -> Kerchief (Deck -> Deck)
promptEditCard card = do
    putStrLn $ showCard card
    newFront <- prompt "Front: "
    newBack  <- prompt "Back: "

    askYesNo "Really edit card? (y/n) "
             (modifyCard card (setCardContents newFront newBack) <$ putStrLn "Card edited.")
             (id <$ putStrLn "Card not edited.")

-- index supplied is 1-based
handleEditIndex :: Int -> Kerchief ()
handleEditIndex n = getDeck >>= maybe printNoDeckLoadedError promptIndex
  where
    promptIndex :: Deck -> Kerchief ()
    promptIndex deck = promptIndex' >>= setDeck . ($ deck)
      where 
        promptIndex' :: Kerchief (Deck -> Deck)
        promptIndex' = maybe (putStrLn "Please pick a valid integer." >> return id)
                             promptEditCard
                             (S.safeElemAt (n-1) $ deckCards deck)
