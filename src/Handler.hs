module Handler (handleInput) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Handler.Add
import Handler.AddFile
import Handler.Decks
import Handler.Edit
import Handler.Exit
import Handler.Load
import Handler.Print
import Handler.Remove
import Handler.RemoveDeck
import Handler.Save
import Handler.Study
import Kerchief

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"    : xs -> handleAdd xs
    "addf"   : xs -> handleAddFile xs
    "decks"  : xs -> handleDecks xs
    "edit"   : xs -> handleEdit xs
    "exit"   : xs -> handleExit xs
    "load"   : xs -> handleLoad xs
    "print"  : xs -> handlePrint xs
    "rename" : xs -> putStrLn "TODO"
    "rm"     : xs -> handleRemove xs
    "rmdeck" : xs -> handleRemoveDeck xs
    "save"   : xs -> handleSave xs
    "study"  : xs -> handleStudy xs
    _             -> putStrLn "Unknown command"
