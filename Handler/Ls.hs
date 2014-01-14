module Handler.Ls (handleLs) where

handleLs :: [String] -> IO ()
handleLs []             = putStrLn filesStr
handleLs ["decks/"]     = putStrLn "TODO"
handleLs [file]
    | file `elem` files = putStrLn file
    | otherwise         = putStrLn "No such file or directory"
handleLs _              = putStrLn "Usage: ls [file]"

filesStr :: String
filesStr = "add  decks/  remove"

files :: [String]
files = words filesStr
