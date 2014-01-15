module Handler.Ls (handleLs) where

import System.Directory (getDirectoryContents)
import Utils (catchVoid)

handleLs :: [String] -> IO ()
handleLs []             = putStrLn filesStr
handleLs ["decks"]      = handleLsDecks
handleLs ["decks/"]     = handleLsDecks
handleLs [file]
    | file `elem` files = putStrLn file
    | otherwise         = putStrLn "No such file or directory"
handleLs _              = putStrLn "Usage: ls [file]"

filesStr :: String
filesStr = "add  deck  decks/  remove"

files :: [String]
files = words filesStr

handleLsDecks :: IO ()
handleLsDecks = catchVoid $
    getDirectoryContents "~/.kerchief" >>= mapM_ putStrLn
