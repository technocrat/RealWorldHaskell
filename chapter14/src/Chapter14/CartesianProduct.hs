module Chapter14.CartesianProduct where

comprehensive :: [a] -> [b] -> [(a, b)]
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]

monadic :: [a] -> [b] -> [(a, b)]
monadic xs ys = do { x <- xs; y <- ys; return (x, y) }

blockyDo :: [a] -> [b] -> [(a, b)]
blockyDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

blockPlain :: [a] -> [b] -> [(a, b)]
blockPlain xs ys =
    xs >>= \x ->
    ys >>= \y ->
    return (x, y)

blockPlain_reloaded :: [a] -> [b] -> [(a, b)]
blockPlain_reloaded xs ys =
    concat (map (\x ->
                concat (map (\y ->
                             return (x, y))
                        ys))
            xs)
