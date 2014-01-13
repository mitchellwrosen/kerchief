module Handler ( handleInput ) where

import Control.Monad.Trans (liftIO)

import Handler.Add
import Handler.Exit
import Handler.Ls
import Handler.Remove
import Kerchief

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"    : xs -> handleAdd xs
    "exit"   : xs -> handleExit xs
    "ls"     : xs -> liftIO $ handleLs xs
    "remove" : xs -> handleRemove xs
    _             -> liftIO $ putStrLn "Unknown command"
    {-| "edit"   `isPrefixOf` line = handleEdit   line-}
    {-| "print"  `isPrefixOf` line = handlePrint  line-}
