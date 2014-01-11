{-# LANGUAGE OverloadedStrings #-}

module Dictionary.Internal where

import           Control.Monad              (mzero)
import           Control.Applicative
import           Data.Aeson
import           Data.List                  (dropWhileEnd)
import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import qualified Data.ByteString.Lazy.Char8 as BS

-- | Write response json to the specified file for inspection.
writeResponseDebug :: String -> FilePath -> IO ()
writeResponseDebug word path = do
    let url = "http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q=" ++ word
    contents <- dropWhileEnd (/= '}') . drop 2 <$> getJson url
    writeFile path contents

getResponse :: String -> IO (Either String Response)
getResponse word =
    let url = "http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q=" ++ word
    in eitherDecode . BS.pack . fixBadHexEscapes . trimResponse <$> getJson url
  where
    -- | Trim off the boiler plate callback characters, because JSONP is returned.
    -- Hard-code "2" because "callback=a" is also hard-coded, so the first two
    -- characters are "a("
    trimResponse :: String -> String
    trimResponse = dropWhileEnd (/= '}') . drop 2

-- | Change "\x" to "\\x" cause Google is dumb. This is super slow.
fixBadHexEscapes :: String -> String
fixBadHexEscapes ('\\':'x':xs) = "\\\\x" ++ fixBadHexEscapes xs
fixBadHexEscapes (x:xs) = x : fixBadHexEscapes xs
fixBadHexEscapes [] = []

getJson :: String -> IO String
getJson url = simpleHTTP (getRequest url) >>= getResponseBody

data Response = Response
    { responsePrimaries      :: [Primary]
    , responseQuery          :: String
    , responseSourceLanguage :: String
    , responseTargetLanguage :: String
    } deriving Show

instance FromJSON Response where
    parseJSON (Object o) = Response <$>
        o .: "primaries"      <*>
        o .: "query"          <*>
        o .: "sourceLanguage" <*>
        o .: "targetLanguage"
    parseJSON _ = mzero

data Primary = Primary
    { primaryEntries :: [PEntry]
    , primaryTerms   :: [Term]
    , primaryType    :: PrimaryType
    } deriving Show

instance FromJSON Primary where
    parseJSON (Object o) = Primary <$>
        o .: "entries" <*>
        o .: "terms"   <*>
        o .: "type"
    parseJSON _ = mzero

data PEntry = PEntry
    { pentryEntries :: Maybe [EEntry]
    , pentryTerms   :: [Term] -- Do these ever have labels?
    , pentryType    :: PEntryType
    } deriving Show

instance FromJSON PEntry where
    parseJSON (Object o) = PEntry <$>
        o .:? "entries" <*>
        o .:  "terms"   <*>
        o .:  "type"
    parseJSON _ = mzero

data EEntry = EEntry
    { eentryTerms :: [Term]
    , eentryType  :: EEntryType
    } deriving Show

instance FromJSON EEntry where
    parseJSON (Object o) = EEntry <$>
        o .: "terms"   <*>
        o .: "type"
    parseJSON _ = mzero

data Term = Term
    { termLabels   :: Maybe [Label]
    , termLanguage :: String
    , termText     :: String
    , termType     :: TermType
    } deriving Show

instance FromJSON Term where
    parseJSON (Object o) = Term <$>
        o .:? "labels"   <*>
        o .:  "language" <*>
        o .:  "text"     <*>
        o .:  "type"
    parseJSON _ = mzero

data Label = Label
    { labelText  :: String
    , labelTitle :: Maybe String
    } deriving Show

instance FromJSON Label where
    parseJSON (Object o) = Label <$>
        o .:  "text" <*>
        o .:? "title"
    parseJSON _ = mzero

data PrimaryType = PHeadword
                 deriving Show

instance FromJSON PrimaryType where
    parseJSON (String "headword") = pure PHeadword
    parseJSON _ = mzero

data PEntryType = PEMeaning
                | PERelated
                deriving Show

instance FromJSON PEntryType where
    parseJSON (String "meaning") = pure PEMeaning
    parseJSON (String "related") = pure PERelated
    parseJSON _ = mzero

data EEntryType = EEExample
                deriving Show

instance FromJSON EEntryType where
    parseJSON (String "example") = pure EEExample
    parseJSON _ = mzero

data TermType = TText
              | TPhonetic
              | TSound
              deriving Show

instance FromJSON TermType where
    parseJSON (String "text")     = pure TText
    parseJSON (String "phonetic") = pure TPhonetic
    parseJSON (String "sound")    = pure TSound
    parseJSON _ = mzero
