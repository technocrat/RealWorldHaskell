module Chapter20.PosixTime where

import           Data.Time             (UTCTime (..))
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Posix.Files
import           System.Posix.Types

-- | Given a path, returns (atime, atime, atime)
getTimes :: FilePath -> IO (UTCTime, UTCTime, UTCTime)
getTimes fp = do
  stat <- getFileStatus fp
  return ( toct (accessTime stat)
         , toct (modificationTime stat)
         , toct (statusChangeTime stat)
         )

-- | Convert an EpochTime to UTCTime
toct :: EpochTime -> UTCTime
toct = posixSecondsToUTCTime . realToFrac
