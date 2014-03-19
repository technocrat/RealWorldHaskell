module Chapter19.DivBy3 where

divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator = map worker
    where worker 0 = Nothing
          worker x = Just (numerator `div` x)
