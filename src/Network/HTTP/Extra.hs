{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Extra 
    ( ConnError
    , getResponseBody'
    ) where

import Control.Applicative
import Network.HTTP
import Network.Stream
import Network.URI

-- | Get-request and get-response all in one.
getResponseBody' :: forall a. HStream a => String -> IO (Maybe a)
getResponseBody' = maybe (return Nothing) withURI . parseURI
  where
    withURI :: URI -> IO (Maybe a)
    withURI uri = either (const Nothing) Just <$> withURI' uri

    withURI' :: HStream a => URI -> IO (Either ConnError a)
    withURI' uri = fmap rspBody <$> simpleHTTP (defaultGETRequest_ uri)
