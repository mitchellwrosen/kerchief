{-# LANGUAGE LambdaCase #-}

module Handler.Add (handleAdd) where

import Control.Applicative          ((<$>), (<*>), (<|>), (<$), (*>), many)
import Control.Monad                (join)
import Network.API.GoogleDictionary (Entry(..), lookupWord)
import Text.Parsec                  (ParseError, parse)
import Text.Parsec.Char             (char, noneOf, spaces)
import Text.Parsec.Combinator       (between, choice)
import Text.Parsec.String           (Parser)

import Card                         (Card, newCard, reverseCard)
import Deck
import Kerchief
import Utils                        (askYesNo, io, printNumberedWith, reads')

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
handleAddWord word = do
    loaded <- isDeckLoaded
    if loaded
        then io (lookupWord word) >>= undefined -- selectEntry
        else io $ putStrLn "No deck loaded. See \"load --help\"."

selectEntry :: [Entry] -> Kerchief ()
selectEntry [] = io . putStrLn $ "No definition found."
selectEntry es = do
    io $ printNumberedWith (\(Entry word def mpos phonetic _) -> 
        word ++ " " ++ phonetic ++ maybe " " (\pos -> " (" ++ pos ++ ") ") mpos ++ def) es
    io $ putStrLn "Which definition? (\"-\" to go back)"
    io getLine >>= \case
        "-" -> io $ putStrLn "No card added."
        s   -> maybe badInput selectEntry' (reads' s)
  where
    selectEntry' :: Int -> Kerchief ()
    selectEntry' n
        | n < 1 || n > length es = badInput
        | otherwise = doAddCard (entryToFront entry) (entryDefinition entry) (entrySoundUrl entry)
      where
        entry :: Entry
        entry = es !! (n-1)

    badInput :: Kerchief ()
    badInput = io (putStrLn "Please pick a valid integer.") >> selectEntry es

doAddCard :: String -> String -> Maybe String -> Kerchief ()
doAddCard front back soundUrl = do
    card <- io $ newCard front soundUrl back Nothing
    doAddCard' card
    askYesNo "Add reverse card as well? (y/n) "
             (doAddCard' $ reverseCard card)
             (return ())
  where
    doAddCard' :: Card -> Kerchief ()
    doAddCard' card = do
        modifyDeck $ addCard card
        io $ putStrLn "Card added."

handleAddFrontBack :: String -> Kerchief ()
handleAddFrontBack = either left right . parse parseFrontBack ""
  where
    left :: ParseError -> Kerchief ()
    left _ = io printAddUsage

    right :: (String, String) -> Kerchief ()
    right (front, back) = doAddCard front back Nothing

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
