{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Compression.GZip  (compress)
import           Control.Concurrent      (forkIO)
import           Control.Exception       (IOException, handle)
import qualified Data.ByteString.Lazy    as L
import           System.Console.Readline (readline)

main :: IO ()
main = do
  maybeLine <- readline "Enter a file to compress> "
  case maybeLine of
    Nothing   -> return ()  -- user entered EOF
    Just ""   -> return ()  -- treat no name as "want to quit"
    Just name -> do
      handle (\(e :: IOException) -> print e) $ do
        content <- L.readFile name
        forkIO (compressFile name content)
        return ()
      main
        where compressFile path = L.writeFile (path ++ ".gz") . compress
