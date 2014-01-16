module Data.Time.Instances where

import Control.Applicative
import Data.Serialize (Serialize, get, put)
import Data.Time.Clock
import Data.Time.Calendar

instance Serialize UTCTime where
    put (UTCTime day difftime) = put day >> put difftime
    get = UTCTime <$> get <*> get

instance Serialize Day where
    put (ModifiedJulianDay n) = put n
    get = ModifiedJulianDay <$> get

instance Serialize DiffTime where
    put = put . fromEnum
    get = toEnum <$> get
