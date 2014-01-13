{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief 
    ( Kerchief 
    , runKerchief
    , getDeck
    , saveDeck
    , useDeck
    ) where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.State

import Deck

data KerchiefState = KState 
    { _ksDeck :: Maybe Deck
    }
makeLenses ''KerchiefState

newtype Kerchief a = 
    Kerchief { unKerchief :: StateT KerchiefState IO a } 
        deriving (Monad, MonadIO)

runKerchief :: Kerchief a -> IO a
runKerchief = (`evalStateT` KState Nothing) . unKerchief

getDeck :: Kerchief (Maybe Deck)
getDeck = Kerchief $ use ksDeck

useDeck :: Deck -> Kerchief ()
useDeck deck = Kerchief $ ksDeck .= Just deck

saveDeck :: Kerchief ()
saveDeck = liftIO (putStrLn "TODO: actually save deck")
