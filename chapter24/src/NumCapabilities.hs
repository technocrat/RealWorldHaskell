module Main where

import           GHC.Conc           (getNumCapabilities)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  numcap <- getNumCapabilities
  putStrLn $ "command line arguments: " ++ show args
  putStrLn $ "number of cores: " ++ show numcap
