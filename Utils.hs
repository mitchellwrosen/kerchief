{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Utils where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable (Foldable, foldl)
import System.Directory (getDirectoryContents)

import Prelude hiding (foldl)

-- | Print each element of a Foldable, prepended by a number (starting at 1).
printNumbered :: (Show a, Foldable t) => t a -> IO ()
printNumbered = printNumberedWith show

printNumberedWith :: Foldable t => (a -> String) -> t a -> IO ()
printNumberedWith f = mapM_ putStrLn . showNumberedWith f

showNumbered :: (Show a, Foldable t) => t a -> [String]
showNumbered = showNumberedWith show

showNumberedWith :: forall a t. Foldable t => (a -> String) -> t a -> [String]
showNumberedWith f = snd . foldl g (0,[])
  where
    g :: (Int,[String]) -> a -> (Int,[String])
    g (n,ss) a = (n+1, ss ++ [show n ++ ". " ++ f a])

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

whenJust :: Monad m => (a -> m b) -> Maybe a -> m ()
whenJust _ Nothing  = return ()
whenJust f (Just a) = f a >> return ()

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' = fmap (filter (\a -> a /= "." && a/= "..")) . getDirectoryContents
