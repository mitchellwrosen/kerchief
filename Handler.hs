module Handler ( handleInput ) where

import Control.Monad.Trans (liftIO)

import Handler.Add
import Handler.Exit
import Handler.Ls
import Kerchief

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"  : xs -> handleAdd xs
    "exit" : xs -> handleExit xs
    "ls"   : xs -> liftIO $ handleLs xs
    _           -> liftIO $ putStrLn "Unknown command"
    {-| "edit"   `isPrefixOf` line = handleEdit   line-}
    {-| "remove" `isPrefixOf` line = handleRemove line-}
    {-| "print"  `isPrefixOf` line = handlePrint  line-}
