{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Network.HTTP.Extra 
    ( getResponseBody'
    , simpleHTTP'
    ) where

import Network.HTTP
import Network.Stream
import Network.URI

-- | Get-request and get-response all in one. Returns Nothing if there was a
-- ConnError, or if there was a code returned other than 400.
simpleHTTP' :: HStream a => String -> IO (Maybe (Response a))
simpleHTTP' = maybe (return Nothing) withURI . parseURI
  where
    withURI :: HStream a => URI -> IO (Maybe (Response a))
    withURI uri = simpleHTTP (defaultGETRequest_ uri) >>= \case
        Left _ -> return Nothing
        Right response -> case rspCode response of
            (4,0,0) -> return (Just response)
            _       -> return Nothing

-- | Get-request and get-response-body all in one. Returns Nothing if there was
-- a ConnError, or if there was a code returned other than 400.
getResponseBody' :: HStream a => String -> IO (Maybe a)
getResponseBody' = fmap (fmap rspBody) . simpleHTTP'
