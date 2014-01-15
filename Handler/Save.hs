module Handler.Save (handleSave) where

import Control.Lens        ((^.))
import Control.Monad.Trans (liftIO)

import Deck                (deckName)
import Kerchief

handleSave :: [String] -> Kerchief ()
handleSave [] = getDeck >>= maybe
    (liftIO $ putStrLn "No deck loaded.")
    (\deck -> do
        saveDeck
        liftIO (putStrLn $ "\"" ++ deck^.deckName ++ "\" saved."))
handleSave _  = liftIO printSaveUsage

printSaveUsage :: IO ()
printSaveUsage = mapM_ putStrLn
    [ "Usage: save"
    , "save the current deck"
    ]
