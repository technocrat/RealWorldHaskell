module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs)
import           System.Exit

main :: IO ()
main = do
  -- Load the command-line arguments
  args <- getArgs

  -- If we don't have the right amount of args, give an error and abort
  when (length args /= 2) $ do
         putStrLn "Syntax: passwdal filename uid"
         exitFailure

  -- Read the file lazily
  content <- readFile (head args)

  -- Compute the username in pure code
  let username = findByUID content $ read (args !! 1)

  -- Display the result
  case username of Just x -> putStrLn x
                   Nothing -> putStrLn "Could not find that UID"

-- | Given the entire input and a UID, see if we can find a username.
findByUID :: String -> Integer -> Maybe String
findByUID content uid = let al = map parseline . lines $ content
                        in lookup uid al

-- | Convert a colon-separated line into fields
parseline :: String -> (Integer, String)
parseline input = let fields = split ':' input
                  in (read (fields !! 2), head fields)

{- | Take a delimeter and a list. Break up the list based on the
   delimeter.
-}
split :: Eq a => a -> [a] -> [[a]]

-- If the input is empty, the result is a list of empty lists.
split _     []  = [[]]
split delim str =
    let -- Find the part of the list before delim and put it in "before".
        -- The rest of the list, including the leading delim, goes
        -- in "remainder".
        (before, remainder) = span (/= delim) str
    in before : case remainder of
                  [] -> []
                  x  -> -- If there is more data to process,
                        -- call split recursively to process it
                        split delim (tail x)
