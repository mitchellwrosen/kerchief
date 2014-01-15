module Handler ( handleInput ) where

import Control.Monad.Trans (liftIO)

import Handler.Add
import Handler.Exit
import Handler.Ls
import Handler.Load
import Handler.Print
import Handler.Remove
import Handler.Save
import Kerchief

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"    : xs -> handleAdd xs
    --"edit"   : xs -> handleEdit xs
    "exit"   : xs -> handleExit xs
    "ls"     : xs -> liftIO $ handleLs xs
    "load"   : xs -> handleLoad xs
    "print"  : xs -> handlePrint xs
    "remove" : xs -> handleRemove xs
    "save"   : xs -> handleSave xs
    _             -> liftIO $ putStrLn "Unknown command"
