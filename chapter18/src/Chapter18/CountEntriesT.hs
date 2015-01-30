module Chapter18.CountEntriesT (listDirectory, countEntries) where

import           Control.Monad              (forM_, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Writer (WriterT, tell)
import           System.Directory           (doesDirectoryExist)
import           System.FilePath            ((</>))

import           Chapter18.CountEntries     (listDirectory)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do let newName = path </> name
                               isDir <- liftIO . doesDirectoryExist $ newName
                               when isDir $ countEntries newName
