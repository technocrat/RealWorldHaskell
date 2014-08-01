module Main where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import           System.Environment
import           Text.Printf

main :: IO ()
main = do [d] <- map read `fmap` getArgs
          print d
          printf "%f\n" (mean (U.enumFromN 1 d))

data Pair = Pair !Int !Double

mean :: Vector Double -> Double
mean xs = s / fromIntegral n
    where Pair n s         = U.foldl' k (Pair 0 0) xs
          k (Pair n' s') x = Pair (n' + 1) (s' + x)
