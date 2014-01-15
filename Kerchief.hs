{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , getDeck
    , isDeckLoaded
    , loadDeck
    , loadDeckByName
    , modifyDeck
    , modifyDeckIO
    , runKerchief
    , saveDeck
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO, liftIO)
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS
import           Data.Serialize            (encode, decode)
import           System.FilePath           ((</>))
import           System.IO

import Config (kerchiefDir)
import Deck
import Utils  (catchNothing, eitherToMaybe, whenJust)

data KerchiefState = KState
    { _ksDeck :: Maybe Deck }
makeLenses ''KerchiefState

newtype Kerchief a =
    Kerchief { unKerchief :: StateT KerchiefState IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadState KerchiefState)

runKerchief :: Kerchief a -> IO a
runKerchief = (`evalStateT` KState Nothing) . unKerchief

isDeckLoaded :: Kerchief Bool
isDeckLoaded = maybe False (const True) <$> getDeck

-- | Get the current deck.
getDeck :: Kerchief (Maybe Deck)
getDeck = use ksDeck

-- | Overwrite the current deck without saving it.
loadDeck :: Deck -> Kerchief ()
loadDeck deck = ksDeck .= Just deck

-- | Modify the current deck.
modifyDeck :: (Deck -> Deck) -> Kerchief ()
modifyDeck f = getDeck >>= maybe (return ()) (loadDeck . f)

-- | Modify the current deck with an IO action.
modifyDeckIO :: (Deck -> IO Deck) -> Kerchief ()
modifyDeckIO f = getDeck >>= maybe (return ()) (\d -> liftIO (f d) >>= loadDeck)

-- | Load the given deck, given its name. Return whether or not the load was
-- successful (i.e. does the deck exist?). Returns True in the case that the
-- deck was already loaded.
loadDeckByName :: String -> Kerchief Bool
loadDeckByName name = getDeck >>= \case
    Nothing -> loadDeckByName'
    Just (Deck name' _ _)
        | name == name' -> return True
        | otherwise     -> loadDeckByName'
  where
    loadDeckByName' :: Kerchief Bool
    loadDeckByName' = liftIO (readDeck name) >>= maybe (return False) (\d -> loadDeck d >> return True)

-- | Read a deck from file, by deck name.
readDeck :: String -> IO (Maybe Deck)
readDeck name = kerchiefDir >>= catchNothing . fmap (eitherToMaybe . decode) . BS.readFile . (</> name)

-- | Save the current deck to file, creating the file first if it doesn't exist.
-- If there is no current deck, do nothing.
saveDeck :: Kerchief ()
saveDeck = getDeck >>= whenJust (liftIO . f)
  where
    f :: Deck -> IO ()
    f deck@(Deck name _ _) = do
        path <- (</> name) <$> kerchiefDir
        withBinaryFile path WriteMode (`BS.hPut` encode deck)
