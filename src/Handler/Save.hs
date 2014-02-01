module Handler.Save (handleSave) where

import Kerchief.Prelude
import Prelude hiding (putStrLn)

import Deck                (deckName)
import Handler.Utils       (printNoDeckLoadedError)
import Kerchief            (Kerchief, getDeck, saveDeck)

handleSave :: [String] -> Kerchief ()
handleSave [] = getDeck >>= maybe
    printNoDeckLoadedError
    (\deck -> saveDeck >> putStrLn ("\"" ++ deck^.deckName ++ "\" saved."))
handleSave _  = printSaveUsage

printSaveUsage :: Kerchief ()
printSaveUsage = mapM_ putStrLn
    [ "Usage: save"
    , "save the current deck"
    ]
