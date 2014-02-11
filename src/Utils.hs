{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Kerchief.Prelude
import Prelude hiding (foldl, getLine, putStr, putStrLn)

import           Control.Exception   (SomeException, catch)
import           Control.Monad.Trans (MonadIO)
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Foldable       (Foldable, foldl)
import           System.Directory    (getDirectoryContents)

askYesNo :: MonadIO m => String -> m a -> m a -> m a
askYesNo s yes no = io (prompt s) >>= \s -> case s of
    "y"   -> yes
    "Y"   -> yes
    "yes" -> yes
    "n"   -> no
    "N"   -> no
    "no"  -> no
    _     -> putStrLn "Please input \"y\" or \"n\"." >> askYesNo s yes no

catchNothing :: IO (Maybe a) -> IO (Maybe a)
catchNothing = (`catch` (\(_ :: SomeException) -> return Nothing))

catchVoid :: IO () -> IO ()
catchVoid = (`catch` (\(_ :: SomeException) -> return ()))

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' = fmap (filter (\a -> a /= "." && a/= "..")) . getDirectoryContents

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f

-- | maybeThen a f m performs action |a| unconditionally, possibly preceded by
-- action |f b| if |m| is Just b.
maybeThen :: Monad m => m a -> (b -> m c) -> Maybe b -> m a
maybeThen thn _ Nothing  = thn
maybeThen thn f (Just b) = f b >> thn

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM (select p) ([],[])
  where
    select :: Monad m => (a -> m Bool) -> ([a],[a]) -> a -> m ([a],[a])
    select q (ts,fs) x =
        ifM (q x)
            (return ((x:ts),fs))
            (return (ts,(x:fs)))

-- | Print each element of a Foldable, prepended by a number (starting at 1).
printNumbered :: (Show a, Foldable t) => t a -> IO ()
printNumbered = printNumberedWith show

printNumberedWith :: (MonadIO m, Foldable t) => (a -> String) -> t a -> m ()
printNumberedWith f = mapM_ putStrLn . showNumberedWith f

prompt :: MonadIO m => String -> m String
prompt s = putStr s >> getLine

showNumbered :: (Show a, Foldable t) => t a -> [String]
showNumbered = showNumberedWith show

showNumberedWith :: forall a t. Foldable t => (a -> String) -> t a -> [String]
showNumberedWith f = snd . foldl g (1,[])
  where
    g :: (Int,[String]) -> a -> (Int,[String])
    g (n,ss) a = (n+1, ss ++ [show n ++ ". " ++ f a])

reads' :: Read a => String -> Maybe a
reads' s = case reads s of
    [(a,"")] -> Just a
    _ -> Nothing

unless' :: Monad m => m () -> Bool -> m ()
unless' = flip unless

whenJust :: Monad m => (a -> m b) -> Maybe a -> m ()
whenJust _ Nothing  = return ()
whenJust f (Just a) = f a >> return ()

safeReadFile :: FilePath -> IO (Maybe ByteString)
safeReadFile path = catchNothing (Just <$> BS.readFile path)
