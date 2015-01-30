module Chapter18.UglyStack where

import           Chapter18.CountEntriesT

import           Control.Monad              (forM, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           System.Directory
import           System.FilePath

data AppConfig = AppConfig { cfgMaxDepth :: Int } deriving Show

data AppState = AppState { stDeepestReached :: Int } deriving Show

-- | Haskell does not allow us to partially apply a type synonym.
--
-- >>> type App2 a = ReaderT AppConfig (StateT AppState IO) a
--
-- When we create a monad transformer stack, we usually wrap it with a newtype.
type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth = let config = AppConfig maxDepth
                        st = AppState 0
                    in runStateT (runReaderT k config) st

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- (liftIO . listDirectory) path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- (liftIO . doesDirectoryExist) newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do let newDepth = curDepth + 1
              st <- lift get
              when (stDeepestReached st < newDepth) $
                (lift . put) st { stDeepestReached = newDepth }
              constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest
