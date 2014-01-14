{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , getDeck
    , isDeckLoaded
    , loadDeck
    , modifyDeck
    , modifyDeckIO
    , runKerchief
    , saveDeck
    , setDeck
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO, liftIO)
import           Control.Monad.Trans.State
import           Data.Set                  (Set)
import qualified Data.Set                  as S

import Deck (Deck)
import qualified Deck

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

isDeckLoaded :: Kerchief Bool
isDeckLoaded = maybe False (const True) <$> getDeck

getDeck :: Kerchief (Maybe Deck)
getDeck = use ksCurrentDeck

-- | Set the current deck.
setDeck :: Deck -> Kerchief ()
setDeck deck = ksCurrentDeck .= Just deck 

modifyDeck :: (Deck -> Deck) -> Kerchief ()
modifyDeck f = getDeck >>= maybe (return ()) (setDeck . f)

modifyDeckIO :: (Deck -> IO Deck) -> Kerchief ()
modifyDeckIO f = getDeck >>= maybe (return ()) (\d -> liftIO (f d) >>= setDeck)

-- | Load the given deck (if necessary) and set it as the current deck.
-- TODO: Actually load the deck from file.
loadDeck :: String -> Kerchief Bool
loadDeck name = do
    let deck = Just (Deck.newDeck name)
    liftIO $ putStrLn ("\"" ++ name ++ "\" loaded.")
    ksCurrentDeck .= deck
    return True

saveDeck :: Kerchief ()
saveDeck = liftIO $ putStrLn "TODO: actually save deck"
