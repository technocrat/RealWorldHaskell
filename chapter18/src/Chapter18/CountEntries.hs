module Chapter18.CountEntries where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM, liftM)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.FilePath     ((</>))

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  -- where notDots p = p /= "." && p /= ".."
  where notDots = (&&) <$> (/=".") <*> (/="..")

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do let newName = path </> name
                                      isDir <- doesDirectoryExist newName
                                      if isDir
                                        then countEntriesTrad newName
                                        else return []
  return $ (path, length contents) : concat rest
