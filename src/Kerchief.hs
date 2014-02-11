{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , getDeck
    , getDecksDir
    , getKerchiefDir
    , getSoundbytesDir
    , isDeckLoaded
    , isModified
    , loadDeck
    , readSoundbyte
    , runKerchief
    , saveDeck
    , saveSoundbyte
    , setDeck
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO)
import           Control.Monad.Trans.State
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Serialize            (encode, decode)
import           System.Directory          (getHomeDirectory)
import           System.FilePath           ((</>))
import           System.IO

import           Deck
import           Kerchief.Prelude          (io)
import           Utils                     (catchNothing, eitherToMaybe, safeReadFile, whenJust)

data KerchiefConfig = KConfig
    { kcDir :: FilePath
    }

data KerchiefState = KState
    { _ksDeck     :: Maybe Deck
    , _ksModified :: Bool -- Keep track of in-memory changes to current deck.
    }
makeLenses ''KerchiefState

newtype Kerchief a =
    Kerchief { unKerchief :: ReaderT KerchiefConfig (StateT KerchiefState IO) a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadState KerchiefState, MonadReader KerchiefConfig)

runKerchief :: Kerchief a -> IO a
runKerchief (Kerchief r) = do
    config <- initKerchiefConfig
    evalStateT (runReaderT r config) initKerchiefState

initKerchiefState :: KerchiefState
initKerchiefState = KState Nothing False

initKerchiefConfig :: IO KerchiefConfig
initKerchiefConfig = KConfig . (</> ".kerchief") <$> getHomeDirectory

isDeckLoaded :: Kerchief Bool
isDeckLoaded = maybe False (const True) <$> getDeck

getKerchiefDir :: Kerchief FilePath
getKerchiefDir = asks kcDir

getDecksDir :: Kerchief FilePath
getDecksDir = (</> "decks") <$> getKerchiefDir

getSoundbytesDir :: Kerchief FilePath
getSoundbytesDir = (</> "soundbytes") <$> getKerchiefDir

-- | Get the current deck.
getDeck :: Kerchief (Maybe Deck)
getDeck = use ksDeck

isModified :: Kerchief Bool
isModified = use ksModified

-- | Overwrite the current deck without saving it.
setDeck :: Deck -> Kerchief ()
setDeck deck = do
    ksDeck     .= Just deck
    ksModified .= True

-- | Load the given deck, given its name. Return the deck if the load
-- was successful (i.e. does the deck exist?).
loadDeck :: String -> Kerchief (Maybe Deck)
loadDeck name = getDeck >>= \case
    Nothing -> loadDeck'
    Just deck
        | name == deck^.deckName -> return (Just deck)
        | otherwise              -> loadDeck'
  where
    loadDeck' :: Kerchief (Maybe Deck)
    loadDeck' = readDeck name >>=
        maybe (return Nothing) (\d -> do
            ksDeck .= Just d
            ksModified .= False
            return (Just d))

-- | Read a deck from file, by deck name. Also update it after reading, since
-- this function is the single function with which decks are read from file.
readDeck :: String -> Kerchief (Maybe Deck)
readDeck name = getDecksDir
    >>= io . catchNothing . fmap (eitherToMaybe . decode) . BS.readFile . (</> name)
    >>= maybe (return Nothing) (fmap Just . io . updateDeck)

-- | Save the current deck to file, creating the file first if it doesn't exist.
-- If there is no current deck, do nothing.
saveDeck :: Kerchief ()
saveDeck = getDeck >>= whenJust saveDeck'
  where
    saveDeck' :: Deck -> Kerchief ()
    saveDeck' deck = do
        path <- (</> deck^.deckName) <$> getDecksDir
        io $ withBinaryFile path WriteMode (`BS.hPut` encode deck)
        ksModified .= False

-- | Save a soundbyte to disk, given its url and contents (mp3 binary).
saveSoundbyte :: String -> ByteString -> Kerchief ()
saveSoundbyte url bytes = getSoundbytePath url >>= io . flip BS.writeFile bytes

-- | Read a soundbyte from disk, given its url.
readSoundbyte :: String -> Kerchief (Maybe ByteString)
readSoundbyte url = getSoundbytePath url >>= io . safeReadFile

-- | Given a url, get its soundbyte path (replace '/' with '-')
getSoundbytePath :: String -> Kerchief FilePath
getSoundbytePath = getSoundbytePath' . map (\c -> if c == '/' then '-' else c)
  where
    getSoundbytePath' :: String -> Kerchief FilePath
    getSoundbytePath' url = (</> url) <$> getSoundbytesDir
