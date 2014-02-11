module Mp3 (playMp3Bytes) where

import Kerchief.Prelude

import           Control.Concurrent (forkIO)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           System.FilePath    ((</>))
import           System.IO          -- (hClose)
import           System.IO.Silently (hSilence)
import           System.Process

-- | Exec hard-coded "mpg123 -" with given bytes.
playMp3Bytes :: ByteString -> IO ()
playMp3Bytes bytes = void . forkIO . hSilence' [stdout, stderr] $ do
    (Just hin, _, _, hprocess) <- createProcess (shell "mpg123 -") { std_in = CreatePipe }
    BS.hPut hin bytes
    hClose hin
    void $ waitForProcess hprocess

-- hSilence doesn't preserve BufferMode of handles it silences.
hSilence' :: [Handle] -> IO a -> IO a
hSilence' hs action = do
    bufferings <- mapM hGetBuffering hs
    result <- hSilence hs action
    mapM_ (uncurry hSetBuffering) (zip hs bufferings)
    return result
