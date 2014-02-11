module Kerchief.Prelude 
    ( io
    , getLine
    , putStr
    , putStrLn
    , module REEXPORT
    ) where

import Control.Applicative as REEXPORT
import Control.Monad       as REEXPORT hiding (mapM_)
import Control.Lens        as REEXPORT

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
