module Handler.Add (handleAdd) where

import Kerchief.Prelude

import Network.API.GoogleDictionary (Entry(..), lookupWord)
import Text.Parsec                  (ParseError, parse)
import Text.Parsec.Char             (char, noneOf, spaces)
import Text.Parsec.Combinator       (between, choice)
import Text.Parsec.String           (Parser)

import Card                         (Card, newCard, reverseCard)
import Deck
import Handler.Utils                (printNoDeckLoadedError)
import Kerchief
import Utils                        (askYesNo, printNumberedWith, reads')

import Prelude hiding (getLine, putStr, putStrLn)

handleAdd :: [String] -> Kerchief ()
handleAdd ["--help"]  = io printAddUsage
handleAdd [word]      = handleAddWord word
handleAdd xs          = handleAddFrontBack (join xs)

printAddUsage :: IO ()
printAddUsage = mapM_ putStrLn
    [ "Usage: add [word|\"front text\" \"back text\"]"
    , "   add word"
    , "      look up |word|, pick a definition, and add it to the current deck"
    , "   add \"front text\" \"back text\""
    , "      add a new card with front |front text| and back |back text|"
    ]

handleAddWord :: String -> Kerchief ()
handleAddWord word = getDeck >>= maybe printNoDeckLoadedError (handleAddWord' word)

handleAddWord' :: String -> Deck -> Kerchief ()
handleAddWord' word deck = io (lookupWord word) >>= selectEntry
  where
    selectEntry :: [Entry] -> Kerchief ()
    selectEntry [] = putStrLn "No definition found."
    selectEntry es = do
        printNumberedWith (\(Entry word def mpos phonetic _) -> 
            word ++ " " ++ phonetic ++ maybe " " (\pos -> " (" ++ pos ++ ") ") mpos ++ def) es
        putStrLn "Which definition? (\"-\" to go back)"
        getLine >>= \ms -> case ms of
            "-" -> putStrLn "No card added."
            s   -> maybe badInput selectEntry' (reads' s)
      where
        selectEntry' :: Int -> Kerchief ()
        selectEntry' n
            | n < 1 || n > length es = badInput
            | otherwise = doAddCard (entryToFront entry) (entryDefinition entry) (entrySoundUrl entry) deck
          where
            entry :: Entry
            entry = es !! (n-1)

        badInput :: Kerchief ()
        badInput = putStrLn "Please pick a valid integer."

doAddCard :: String -> String -> Maybe String -> Deck -> Kerchief ()
doAddCard front back soundUrl deck = do
    card <- io $ newCard front back soundUrl
    putStrLn "Card added."

    let deck' = addCard card deck
    askYesNo "Add reverse card as well? (y/n) "
             (return $ addCard (reverseCard card) deck') -- Either add two cards to the deck...
             (return                              deck') -- ...or only one
        >>= setDeck

handleAddFrontBack :: String -> Kerchief ()
handleAddFrontBack line = getDeck >>= maybe printNoDeckLoadedError (handleAddFrontBack' line)

handleAddFrontBack' :: String -> Deck -> Kerchief ()
handleAddFrontBack' line deck = either left right . parse parseFrontBack "" $ line
  where
    left :: ParseError -> Kerchief ()
    left _ = io printAddUsage

    right :: (String, String) -> Kerchief ()
    right (front, back) = doAddCard front back Nothing deck

parseFrontBack :: Parser (String, String)
parseFrontBack = (,) <$> parseQuotedString <*> (spaces *> parseQuotedString)

parseQuotedString :: Parser String
parseQuotedString = quoted $ many stringChar
  where
    quoted :: Parser a -> Parser a
    quoted = between (char '\"') (char '\"')

    stringChar :: Parser Char
    stringChar = escapedChar <|> noneOf "\""

    escapedChar :: Parser Char
    escapedChar = char '\\' *> choice (zipWith escape codes replacements)

    escape :: Char -> Char -> Parser Char
    escape code replacement = replacement <$ char code

    codes, replacements :: [Char]
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

entryToFront :: Entry -> String
entryToFront (Entry word defn mpos phonetic _) = word ++ " " ++ phonetic ++ maybe "" (\pos -> " (" ++ pos++")") mpos
