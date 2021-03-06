module Chapter14.Logger
    ( Logger
    , Log
    , runLogger
    , record
    ) where

import           Control.Monad (liftM, liftM2)

newtype Logger a = Logger { execLogger :: (a, Log) }

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds ->
                 return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return $ '.':ds
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return $ ".*" ++ ds
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return $ "[^" ++ c : ds
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return $ "[" ++ c : ds
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c | c `elem` regexChars = record "escape" >> return ['\\',c]
         | otherwise           = return [c]
         where regexChars = "\\+()^$.{}]|"

charClass_wordy :: String -> Logger String
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return $ ']':ds
charClass_wordy (c:cs) =
    charClass_wordy cs >>= \ds ->
    return $ c:ds

charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs)   = (c:) `liftM` charClass cs
