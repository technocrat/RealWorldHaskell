module Chapter9.ControlledVisit where

import           Control.Exception (bracket, handle)
import           Control.Monad     (forM, liftM)
import           Data.Time.Clock   (UTCTime (..))
import           System.Directory  (Permissions (..), getDirectoryContents,
                                    getModificationTime, getPermissions)
import           System.FilePath   ((</>))
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

data Info = Info { infoPath    :: FilePath
                 , infoPerms   :: Maybe Permissions
                 , infoSize    :: Maybe Integer
                 , infoModTime :: Maybe UTCTime
                 } deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names    <- getUsefulContents path
  contents <- mapM getInfo $ path : map (path </>) names
  liftM concat $ forM (order contents) $ \info ->
      if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return $ filter (`notElem` [".", ".."]) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a))
              (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO $ getPermissions path
  size  <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
  modified <- maybeIO $ getModificationTime path
  return $ Info path perms size modified

traverseVerbose :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseVerbose order path =
    do names <- getDirectoryContents path
       let usefulNames = filter (`notElem` [".", ".."]) names
       contents <- mapM getEntryName ("" : usefulNames)
       recursiveContents <- mapM recurse (order contents)
       return $ concat recursiveContents
    where getEntryName name = getInfo (path </> name)
          isDirectory' info = case infoPerms info of
                                Nothing    -> False
                                Just perms -> searchable perms
          recurse info = if isDirectory' info && infoPath info /= path
                         then traverseVerbose order (infoPath info)
                         else return [info]
