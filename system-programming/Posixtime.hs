import System.Posix.Files
import Data.Time
import System.Posix.Types

-- Given a path, returns (atime, mtime, ctime)
getTimes :: FilePath -> IO (UniversalTime, UniversalTime, UniversalTime)
getTimes fp =
    do stat <- getFileStatus fp
       return (toct (accessTime stat),
               toct (modificationTime stat),
               toct (statusChangeTime stat))

-- Convert an `EpochTime` to a `UniversalTime`
toct :: EpochTime -> UniversalTime
toct et = ModJulianDate (toRational et)