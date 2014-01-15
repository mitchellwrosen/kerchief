module Config where

import Control.Applicative
import System.FilePath     ((</>))
import System.Directory    (getHomeDirectory)

kerchiefDir :: IO FilePath
kerchiefDir = (</> ".kerchief") <$> getHomeDirectory
