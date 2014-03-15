{-# LANGUAGE CPP #-}

module Main where

#define N 16

main :: IO ()
main = print [1 .. N]
