module Handler.RemoveDeck where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import System.Directory (removeFile)
import System.FilePath  ((</>))

import Kerchief         (Kerchief, getDecksDir)
import Utils            (askYesNo, getDirectoryContents')

handleRemoveDeck :: [String] -> Kerchief ()
handleRemoveDeck ["--help"] = printRemoveDeckUsage
handleRemoveDeck [name]     = handleRemoveDeck' name
handleRemoveDeck _          = printRemoveDeckUsage

handleRemoveDeck' :: String -> Kerchief ()
handleRemoveDeck' name = do
    decksDir <- getDecksDir
    decks    <- io (getDirectoryContents' decksDir)
    if name `elem` decks
        then askYesNo ("Are you sure you want to remove deck \"" ++ name ++ "\"? ")
                      (do
                          io $ removeFile (decksDir </> name)
                          putStrLn $ "Deck \"" ++ name ++ "\" removed.")
                      (putStrLn $ "Deck \"" ++ name ++ "\" not removed.")
        else putStrLn $ "Deck \"" ++ name ++ "\" doesn't exist. See \"decks\"."

printRemoveDeckUsage :: Kerchief ()
printRemoveDeckUsage = putStrLn "TODO"
