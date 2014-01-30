module Kerchief.Prelude 
    ( io
    , getLine
    , putStr
    , putStrLn
    , module Control.Applicative
    , module Control.Monad
    , module Control.Lens
    ) where

import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Lens

import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding (getLine, putStr, putStrLn)
import qualified Prelude

io :: MonadIO m => IO a -> m a
io = liftIO

putStr :: MonadIO m => String -> m ()
putStr = io . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = io . Prelude.putStrLn

getLine :: MonadIO m => m String
getLine = io Prelude.getLine
