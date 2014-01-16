{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Utils where

import           Control.Applicative ((<$>))
import           Control.Exception   (SomeException, catch)
import           Control.Monad       (foldM, unless)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Foldable       (Foldable, foldl)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           System.Directory    (getDirectoryContents)
import           System.Random       (randomRIO)

import Prelude hiding (foldl)

askYesNo :: MonadIO m => String -> m a -> m a -> m a
askYesNo s yes no = liftIO (prompt s) >>= \case
    "y"   -> yes
    "Y"   -> yes
    "yes" -> yes
    "n"   -> no
    "N"   -> no
    "no"  -> no
    _     -> liftIO (putStrLn "Please input \"y\" or \"n\".") >> askYesNo s yes no

catchNothing :: IO (Maybe a) -> IO (Maybe a)
catchNothing = (`catch` (\(_ :: SomeException) -> return Nothing))

catchVoid :: IO () -> IO ()
catchVoid = (`catch` (\(_ :: SomeException) -> return ()))

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- | Safe replacement for Data.Set's partial elemAt
elemAt' :: Int -> Set a -> Maybe a
elemAt' n s 
    | n < 0 || n >= S.size s = Nothing
    | otherwise              = Just (S.elemAt n s)

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

printNumberedWith :: Foldable t => (a -> String) -> t a -> IO ()
printNumberedWith f = mapM_ putStrLn . showNumberedWith f

prompt :: String -> IO String
prompt s = putStr s >> getLine

showNumbered :: (Show a, Foldable t) => t a -> [String]
showNumbered = showNumberedWith show

showNumberedWith :: forall a t. Foldable t => (a -> String) -> t a -> [String]
showNumberedWith f = snd . foldl g (1,[])
  where
    g :: (Int,[String]) -> a -> (Int,[String])
    g (n,ss) a = (n+1, ss ++ [show n ++ ". " ++ f a])

randomElem :: Set a -> IO (Maybe a)
randomElem s 
    | S.null s  = return Nothing
    | otherwise = Just . flip S.elemAt s <$> randomRIO (0, S.size s)

reads' :: Read a => String -> Maybe a
reads' s = case reads s of
    [(a,"")] -> Just a
    _ -> Nothing

unless' :: Monad m => m () -> Bool -> m ()
unless' = flip unless

whenJust :: Monad m => (a -> m b) -> Maybe a -> m ()
whenJust _ Nothing  = return ()
whenJust f (Just a) = f a >> return ()
