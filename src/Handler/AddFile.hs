module Handler.AddFile (handleAddFile) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Kerchief

handleAddFile :: [String] -> Kerchief ()
handleAddFile ["--help"]  = printAddFileUsage
handleAddFile [file]      = handleAddFile' file
handleAddFile _           = printAddFileUsage

printAddFileUsage :: Kerchief ()
printAddFileUsage = mapM_ putStrLn
    [ "Usage: addf [file]"
    , "   add words in |file|, which contains one word per line"
    ]

handleAddFile' :: String -> Kerchief ()
handleAddFile' _ = putStrLn "TODO"
