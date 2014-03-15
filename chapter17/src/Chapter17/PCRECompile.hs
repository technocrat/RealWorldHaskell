{-# LANGUAGE ForeignFunctionInterface #-}

module Chapter17.PCRECompile where

import           Chapter17.Regex
import           Data.ByteString.Char8
import           Data.Word
import           Foreign               hiding (unsafePerformIO)
import           Foreign.C.String
import           Foreign.C.Types
import           System.IO.Unsafe      (unsafePerformIO)

type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                   -> PCREOption
                   -> Ptr CString
                   -> Ptr CInt
                   -> Ptr Word8
                   -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !ByteString
             deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
                    useAsCString str $ \pattern ->
                    alloca $ \errptr    ->
                    alloca $ \erroffset -> do
                      pcre_ptr <- c_pcre_compile pattern (combineOptions flags)
                                  errptr erroffset nullPtr
                      if pcre_ptr == nullPtr
                      then do err <- peekCString =<< peek errptr
                              return $ Left err
                      else do reg <- newForeignPtr finalizerFree pcre_ptr
                              return $ Right (Regex reg str)
