module Handler.Ls (handleLs) where

import Config           (kerchiefDir)
import Utils            (catchVoid, getDirectoryContents')

handleLs :: [String] -> IO ()
handleLs []             = putStrLn filesStr
handleLs ["decks"]      = handleLsDecks
handleLs ["decks/"]     = handleLsDecks
handleLs [file]
    | file `elem` files = putStrLn file
    | otherwise         = putStrLn "No such file or directory"
handleLs _              = putStrLn "Usage: ls [file]"

filesStr :: String
filesStr = "add  deck  decks/  remove  save"

files :: [String]
files = words filesStr

handleLsDecks :: IO ()
handleLsDecks = catchVoid $
    kerchiefDir >>= getDirectoryContents' >>= mapM_ putStrLn
