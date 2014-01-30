module Mp3
    ( playMp3
    , playMp3Url
    ) where

import           Control.Monad      (void)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           System.IO          -- (hClose)
import           System.IO.Silently (hSilence)
import           System.Process

import Network.HTTP.Extra (getResponseBody')

playMp3 :: ByteString -> IO ()
playMp3 bytes = hSilence' [stdout, stderr] $ do
    (Just hin, _, _, hprocess) <- createProcess (shell "mpg123 -") { std_in = CreatePipe }
    BS.hPut hin bytes
    hClose hin
    void $ waitForProcess hprocess

playMp3Url :: String -> IO ()
playMp3Url url = getResponseBody' url >>= maybe (return ()) playMp3

-- hSilence doesn't preserve BufferMode of handles it silences.
hSilence' :: [Handle] -> IO a -> IO a
hSilence' hs action = do
    bufferings <- mapM hGetBuffering hs
    result <- hSilence hs action
    mapM_ (\(h,b) -> hSetBuffering h b) (zip hs bufferings)
    return result
