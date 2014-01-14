{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Utils where

import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable

-- | Print each element of a Foldable, prepended by a number (starting at 1).
printNumbered :: forall a t. (Show a, Foldable t) => t a -> IO ()
printNumbered = void . foldlM f 1
  where
    f :: Int -> a -> IO Int
    f n card = putStrLn (show n ++ ". " ++ show card) >> return (n+1)

prompt :: String -> IO String
prompt s = putStr s >> getLine

askYesNo :: MonadIO m => String -> m a -> m a -> m a
askYesNo s yes no = liftIO (prompt s) >>= \case
    "y"   -> yes
    "Y"   -> yes
    "yes" -> yes
    "n"   -> no
    "N"   -> no
    "no"  -> no
    _     -> liftIO (putStrLn "Please input \"y\" or \"n\".") >> askYesNo s yes no
