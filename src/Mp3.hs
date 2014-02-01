{-# LANGUAGE LambdaCase #-}

module Mp3 (playMp3) where

import Kerchief.Prelude

import           Control.Monad      (void)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           System.IO          -- (hClose)
import           System.IO.Silently (hSilence)
import           System.Process

import Kerchief           (Kerchief)
import Network.HTTP.Extra (getResponseBody')
import Utils              (safeReadFile)

-- | Play the mp3 at the given URL. Checks soundbytesDir before hitting the
-- network.
playMp3 :: String -> Kerchief ()
playMp3 url = readSoundbyte url >>= maybe (downloadSaveAndPlayMp3 url) (io . playMp3Bytes)

downloadSaveAndPlayMp3 :: String -> Kerchief ()
downloadSaveAndPlayMp3 url = io (getResponseBody' url) >>= maybe (return ()) (saveAndPlayMp3 url)

saveAndPlayMp3 :: String -> ByteString -> Kerchief ()
saveAndPlayMp3 url bytes = saveSoundbyte url bytes >> io (playMp3Bytes bytes)

-- | Exec hard-coded "mpg123 -" with given bytes.
playMp3Bytes :: ByteString -> IO ()
playMp3Bytes bytes = hSilence' [stdout, stderr] $ do
    (Just hin, _, _, hprocess) <- createProcess (shell "mpg123 -") { std_in = CreatePipe }
    BS.hPut hin bytes
    hClose hin
    void $ waitForProcess hprocess

-- hSilence doesn't preserve BufferMode of handles it silences.
hSilence' :: [Handle] -> IO a -> IO a
hSilence' hs action = do
    bufferings <- mapM hGetBuffering hs
    result <- hSilence hs action
    mapM_ (\(h,b) -> hSetBuffering h b) (zip hs bufferings)
    return result

saveSoundbyte :: String -> ByteString -> Kerchief ()
saveSoundbyte url bytes = undefined

readSoundbyte :: String -> Kerchief (Maybe ByteString)
readSoundbyte url = undefined
