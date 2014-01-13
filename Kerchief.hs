{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , runKerchief
    , getCurrentDeck
    , loadDeck
    , putDeck
    , saveDeck
    {-, useDeck-}
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO, liftIO)
import           Control.Monad.Trans.State
import           Data.Set                  (Set)
import qualified Data.Set                  as S

import Deck

data KerchiefState = KState 
    { _ksCurrentDeck :: Maybe Deck
    , _ksLoadedDecks :: Set Deck
    }
makeLenses ''KerchiefState

newtype Kerchief a = 
    Kerchief { unKerchief :: StateT KerchiefState IO a } 
        deriving (Functor, Applicative, Monad, MonadIO, MonadState KerchiefState)

runKerchief :: Kerchief a -> IO a
runKerchief = (`evalStateT` KState Nothing S.empty) . unKerchief

getCurrentDeck :: Kerchief (Maybe Deck)
getCurrentDeck = Kerchief $ use ksCurrentDeck

{-useDeck :: Deck -> Kerchief ()-}
{-useDeck deck = Kerchief $ ksDeck .= Just deck-}

-- | Load the given deck (if necessary). Don't set it as the current deck.
loadDeck :: String -> Kerchief (Maybe Deck)
loadDeck name = Nothing <$ liftIO (putStrLn "TODO: actually load deck")

-- | Put the deck into the state, possibly overwriting the contents of another
-- deck (if it already exists).
putDeck :: Deck -> Kerchief ()
putDeck deck = getCurrentDeck >>= maybe no yes
  where
    no    = ksLoadedDecks %= S.insert deck
    yes d = ksCurrentDeck .= Just d

saveDeck :: Kerchief ()
saveDeck = liftIO (putStrLn "TODO: actually save deck")
