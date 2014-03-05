module Main where

import Chapter5.SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
