module Handler.Save (handleSave) where

import Control.Monad.Trans (liftIO)

import Deck          (Deck(..))
import Kerchief

handleSave :: [String] -> Kerchief ()
handleSave [] = getDeck >>= maybe
    (liftIO $ putStrLn "No deck loaded.")
    (\(Deck name _ _) -> do
        saveDeck
        liftIO (putStrLn $ "\"" ++ name ++ "\" saved."))
handleSave _  = liftIO printSaveUsage

printSaveUsage :: IO ()
printSaveUsage = mapM_ putStrLn
    [ "Usage: save"
    , "save the current deck"
    ]
