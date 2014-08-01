module Main where

import           Data.List          (foldl')
import           System.Environment
import           Text.Printf

main :: IO ()
main = do [d] <- map read `fmap` getArgs
          printf "%f\n" (mean [1..d])

data Pair a b = Pair !a !b

mean :: [Double] -> Double
mean xs = s / fromIntegral n
    where Pair n s         = foldl' k (Pair 0 0) xs
          k (Pair n' s') x = Pair (n' + 1) (s' + x)
