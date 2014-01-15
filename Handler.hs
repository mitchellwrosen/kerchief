module Handler ( handleInput ) where

import Control.Monad.Trans (liftIO)

import Handler.Add
import Handler.Deck
import Handler.Exit
import Handler.Ls
import Handler.Remove
import Handler.Save
import Kerchief

handleInput :: String -> Kerchief ()
handleInput line = case words line of
    "add"    : xs -> handleAdd xs
    "deck"   : xs -> handleDeck xs
    "exit"   : xs -> handleExit xs
    "ls"     : xs -> liftIO $ handleLs xs
    "remove" : xs -> handleRemove xs
    "save"   : xs -> handleSave xs
    _             -> liftIO $ putStrLn "Unknown command"
    {-| "edit"   `isPrefixOf` line = handleEdit   line-}
    {-| "print"  `isPrefixOf` line = handlePrint  line-}
