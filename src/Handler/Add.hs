{-# LANGUAGE LambdaCase #-}

module Handler.Add (handleAdd) where

import Control.Applicative          ((<$>), (<*>), (<|>), (<$), (*>), many)
import Control.Monad                (join)
import Network.API.GoogleDictionary (Entry(..), lookupWord)
import Text.Parsec                  (parse)
import Text.Parsec.Char             (char, noneOf, spaces)
import Text.Parsec.Combinator       (between, choice)
import Text.Parsec.String           (Parser)

import Card                         (Card, newCard, nthEntry, reverseCard)
import Deck
import Kerchief
import Utils                        (askYesNo, io, showNumberedWith)

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
        then io (lookupWord word) >>=
            maybe (io . putStrLn $ "No definition found for \"" ++ word ++ "\".")
                  (\entry -> io (putStrLn $ showEntry entry) >> pickDefinition entry)
        else io $ putStrLn "No deck loaded. See \"load --help\"."
  where
    showEntry :: Entry -> String
    showEntry (Entry w d) = unlines $ "" : w : showNumberedWith (\(pos,def) -> "(" ++ pos ++ ") " ++ def) d

    pickDefinition :: Entry -> Kerchief ()
    pickDefinition entry = do
        io $ putStrLn "Which definition? (\"-\" to go back)"
        io getLine >>= \case
            "-" -> io $ putStrLn "No card added."
            s   -> pickDefinition' entry (reads s)

    pickDefinition' :: Entry -> [(Int,String)] -> Kerchief ()
    pickDefinition' entry [(n,"")] = maybe (bad entry) doAddCard (nthEntry (n-1) entry)
    pickDefinition' entry _        = bad entry

    bad :: Entry -> Kerchief ()
    bad entry = io (putStrLn "Please pick a valid integer.") >> pickDefinition entry

doAddCard :: (String, String) -> Kerchief ()
doAddCard (front,back) = do
    card <- io $ newCard front back
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
handleAddFrontBack = either (const $ io printAddUsage) doAddCard . parse parseFrontBack ""

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
