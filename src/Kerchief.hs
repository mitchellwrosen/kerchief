{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Kerchief
    ( Kerchief
    , getDeck
    , isDeckLoaded
    , isModified
    , loadDeck
    {-, modifyDeck-}
    {-, modifyDeckIO-}
    , runKerchief
    , saveDeck
    , setDeck
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State       (MonadState)
import           Control.Monad.Trans       (MonadIO)
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS
import           Data.Serialize            (encode, decode)
import           System.FilePath           ((</>))
import           System.IO

import           Config                    (kerchiefDir)
import           Deck
import           Kerchief.Prelude          (io)
import           Utils                     (catchNothing, eitherToMaybe, whenJust)

data KerchiefState = KState
    { _ksDeck     :: Maybe Deck
    , _ksModified :: Bool -- Keep track of in-memory changes to current deck.
    }
makeLenses ''KerchiefState

newtype Kerchief a =
    Kerchief { unKerchief :: StateT KerchiefState IO a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadState KerchiefState)

runKerchief :: Kerchief a -> IO a
runKerchief = (`evalStateT` KState Nothing False) . unKerchief

isDeckLoaded :: Kerchief Bool
isDeckLoaded = maybe False (const True) <$> getDeck

-- | Get the current deck.
getDeck :: Kerchief (Maybe Deck)
getDeck = use ksDeck

isModified :: Kerchief Bool
isModified = use ksModified

-- | Overwrite the current deck without saving it.
setDeck :: Deck -> Kerchief ()
setDeck deck = do
    ksDeck .= Just deck
    ksModified .= False

-- | Modify the current deck.
{-modifyDeck :: (Deck -> Deck) -> Kerchief ()-}
{-modifyDeck f = getDeck >>= \case-}
    {-Nothing   -> return ()-}
    {-Just deck -> do-}
        {-setDeck (f deck)-}
        {-ksModified .= True-}

{--- | Modify the current deck with an IO action.-}
{-modifyDeckIO :: (Deck -> IO Deck) -> Kerchief ()-}
{-modifyDeckIO f = getDeck >>= \case-}
    {-Nothing -> return ()-}
    {-Just deck -> do-}
        {-io (f deck) >>= setDeck-}
        {-ksModified .= True-}

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
    loadDeck' = io (readDeck name) >>=
        maybe (return Nothing) (\d -> setDeck d >> return (Just d))

-- | Read a deck from file, by deck name. Also update it after reading, since
-- this function is the single function with which decks are read from file.
readDeck :: String -> IO (Maybe Deck)
readDeck name = do
    kerchiefDir
        >>= catchNothing . fmap (eitherToMaybe . decode) . BS.readFile . (</> name)
        >>= maybe (return Nothing) (fmap Just . updateDeck)

-- | Save the current deck to file, creating the file first if it doesn't exist.
-- If there is no current deck, do nothing.
saveDeck :: Kerchief ()
saveDeck = getDeck >>= whenJust saveDeck'
  where
    saveDeck' :: Deck -> Kerchief ()
    saveDeck' deck = do
        path <- (</> deck^.deckName) <$> io kerchiefDir
        io $ withBinaryFile path WriteMode (`BS.hPut` encode deck)
        ksModified .= False
