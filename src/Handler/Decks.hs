module Handler.Decks (handleDecks) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Data.List (intercalate)

import Config    (kerchiefDir)
import Kerchief  (Kerchief)
import Utils     (getDirectoryContents')

handleDecks :: [String] -> Kerchief ()
handleDecks _ = io $ kerchiefDir >>= getDirectoryContents' >>= putStrLn . intercalate " "
