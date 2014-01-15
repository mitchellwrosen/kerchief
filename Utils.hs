{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Utils where

import Control.Exception (SomeException, catch)
import Control.Monad (unless, void)
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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

catchVoid :: IO () -> IO ()
catchVoid = (`catch` (\(_ :: SomeException) -> return ()))

catchNothing :: IO (Maybe a) -> IO (Maybe a)
catchNothing = (`catch` (\(_ :: SomeException) -> return Nothing))

unless' :: Monad m => m () -> Bool -> m ()
unless' = flip unless

-- | maybeThen a f m performs action |a| unconditionally, possibly preceded by
-- action |f b| if |m| is Just b.
maybeThen :: Monad m => m a -> (b -> m c) -> Maybe b -> m a
maybeThen thn _ Nothing  = thn
maybeThen thn f (Just b) = f b >> thn
