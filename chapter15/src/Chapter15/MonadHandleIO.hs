{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter15.MonadHandleIO where

import           Chapter15.MonadHandle
-- import           Control.Monad.Trans   (MonadIO (..), MonadTrans (..))
-- import           System.Directory      (removeFile)
-- import           System.IO             (IOMode (..))
import           Chapter15.SafeHello
import qualified System.IO

instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStr = System.IO.hPutStr
    hClose = System.IO.hClose
    hGetContents = System.IO.hGetContents
    hPutStrLn = System.IO.hPutStrLn
