module Handler.Save (handleSave) where

import Control.Monad.Trans (liftIO)

import Deck          (Deck(..))
import Kerchief
import Utils         (whenJust)

handleSave :: [String] -> Kerchief ()
handleSave ["--help"] = liftIO printSaveUsage
handleSave []         = getDeck >>= whenJust f
  where
    f :: Deck -> Kerchief ()
    f (Deck name _ _) = saveDeck >> liftIO (putStrLn $ "\"" ++ name ++ "\" saved.")
handleSave _            = liftIO printSaveUsage

printSaveUsage :: IO ()
printSaveUsage = mapM_ putStrLn
    [ "Usage: save"
    , ""
    , "save the current deck"
    , ""
    ]
