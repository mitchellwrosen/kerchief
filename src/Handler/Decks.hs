module Handler.Decks (handleDecks) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Kerchief  (Kerchief, getDecksDir)
import Utils     (getDirectoryContents')

handleDecks :: [String] -> Kerchief ()
handleDecks _ = getDecksDir >>= io . getDirectoryContents' >>= putStrLn . unwords
