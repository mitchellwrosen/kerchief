module Handler (handleInput) where

import Handler.Add
import Handler.Edit
import Handler.Exit
import Handler.Load
import Handler.Print
import Handler.Remove
import Handler.Save
import Handler.Study
import Kerchief

import Utils (io)

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"    : xs -> handleAdd xs
    "edit"   : xs -> handleEdit xs
    "exit"   : xs -> handleExit xs
    "load"   : xs -> handleLoad xs
    "print"  : xs -> handlePrint xs
    "rename" : xs -> io (putStrLn "TODO")
    "rm"     : xs -> handleRemove xs
    "rmdeck" : xs -> io (putStrLn "TODO")
    "save"   : xs -> handleSave xs
    "study"  : xs -> handleStudy xs
    _             -> io $ putStrLn "Unknown command"
