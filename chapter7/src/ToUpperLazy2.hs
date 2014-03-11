module Main where

import           Data.Char (toUpper)
import           System.IO

main :: IO ()
main = do
  inh    <- openFile "input.txt" ReadMode
  outh   <- openFile "output.txt" WriteMode
  inpStr <- hGetContents inh
  hPutStr outh (map toUpper inpStr)
  hClose inh
  hClose outh
