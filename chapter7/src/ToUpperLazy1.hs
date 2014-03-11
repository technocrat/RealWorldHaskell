module Main where

import           Data.Char (toUpper)
import           System.IO

main :: IO ()
main = do
  inh    <- openFile "input.txt" ReadMode
  outh   <- openFile "output.txt" WriteMode
  inpStr <- hGetContents inh
  let result = processData inpStr
  hPutStr outh result
  hClose inh
  hClose outh

processData :: String -> String
processData = map toUpper
