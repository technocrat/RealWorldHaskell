module Chapter19.DivBy2m where

divBy :: Integral a => a -> [a] -> Maybe [a]
divBy numerator = mapM (numerator `safeDiv`)
    where safeDiv _ 0 = Nothing
          safeDiv x y = Just (x `div` y)
