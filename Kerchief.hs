{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , getDeck
    , isDeckLoaded
    , loadDeck
    , getLoadedDecks
    , modifyDeck
    , modifyDeckIO
    , readDeck
    , runKerchief
    , setDeck
    , writeDeck
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO, liftIO)
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS
import           Data.Serialize            (encode, decode)
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           System.Directory          (getHomeDirectory)
import           System.FilePath           ((</>))
import           System.IO

import Deck
import Utils (catchNothing, eitherToMaybe)

kerchiefDir :: FilePath -> IO FilePath
kerchiefDir path = (</> ".kerchief" </> path) <$> getHomeDirectory

-- | Invariant: if there are any loaded decks, the current deck is Just.
-- The loadedDecks are thus the previously loaded decks that have been
-- switched out, but are kept in memory. It's not possible to load a deck
-- and have currentDeck be Nothing.
data KerchiefState = KState
    { _ksCurrentDeck :: Maybe Deck
    , _ksLoadedDecks :: Set Deck    -- Not including the current deck.
    }
makeLenses ''KerchiefState

newtype Kerchief a =
    Kerchief { unKerchief :: StateT KerchiefState IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadState KerchiefState)

runKerchief :: Kerchief a -> IO a
runKerchief = (`evalStateT` KState Nothing S.empty) . unKerchief

-- | Check if any decks have been loaded. Per the invariant on KerchiefState,
-- it's sufficient to just check the current deck.
isDeckLoaded :: Kerchief Bool
isDeckLoaded = maybe False (const True) <$> getDeck

-- | Get all loaded decks (including current deck).
getLoadedDecks :: Kerchief (Set Deck)
getLoadedDecks = do
    loaded <- use ksLoadedDecks
    maybe loaded (`S.insert` loaded) <$> use ksCurrentDeck

getDeck :: Kerchief (Maybe Deck)
getDeck = use ksCurrentDeck

-- | Set the current deck to |deck|. Depending on if there is a current deck, or
-- if there are any loaded decks (one of which may be |deck|), this function may
-- do different amounts of data shuffling. At the end, |deck| will be the
-- current deck, and |deck| will not exist in loadedDecks.
setDeck :: Deck -> Kerchief ()
setDeck deck = getDeck >>= maybe setDeck' ifCurrentDeck
  where
    ifCurrentDeck :: Deck -> Kerchief ()
    ifCurrentDeck cur
        | cur == deck = return () -- Current deck == argument, do nothing
        | otherwise   = do
            ksLoadedDecks %= S.insert cur
            setDeck'

    setDeck' :: Kerchief ()
    setDeck' = do
        ksCurrentDeck .= Just deck
        ksLoadedDecks %= S.delete deck

-- | Modify the current deck.
modifyDeck :: (Deck -> Deck) -> Kerchief ()
modifyDeck f = getDeck >>= maybe (return ()) (setDeck . f)

-- | Modify the current deck with an IO action.
modifyDeckIO :: (Deck -> IO Deck) -> Kerchief ()
modifyDeckIO f = getDeck >>= maybe (return ()) (\d -> liftIO (f d) >>= setDeck)

-- | Load the given deck (if necessary) and set it as the current deck. Return
-- whether or not the load was successful (i.e. does the deck exist?). Returns
-- True in the case that the deck was already loaded.
loadDeck :: String -> Kerchief Bool
loadDeck name = getLoadedDecks >>= maybe notAlreadyLoaded alreadyLoaded . Deck.getDeckWithName name
  where
    -- A bit of a misnomer that, in the case of a successful readDeck, this
    -- function delegates to "alreadyLoaded". This is because alreadyLoaded
    -- simply calls setDeck, which is agnostic to whether or not the provided
    -- deck is in loadedDecks (it sets the current deck and deletes from
    -- loadedDecks either way).
    notAlreadyLoaded :: Kerchief Bool
    notAlreadyLoaded = liftIO (readDeck name) >>= maybe (return False) alreadyLoaded

    alreadyLoaded :: Deck -> Kerchief Bool
    alreadyLoaded d = setDeck d >> return True

readDeck :: String -> IO (Maybe Deck)
readDeck name = kerchiefDir name >>= catchNothing . fmap (eitherToMaybe . decode) . BS.readFile

-- | Write this deck to file, creating the file first if it doesn't exist.
writeDeck :: Deck -> IO ()
writeDeck deck = do
    kerchiefDir (deck^.deckName) >>= \path ->
        withBinaryFile path WriteMode $ \handle -> do
            BS.hPut handle (encode deck)
            hClose handle
