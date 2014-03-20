{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Chapter11.Prettify2

import           Data.List             (intersperse)
import           Data.Monoid           hiding ((<>))
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "QuickCheck Tests"
           [ testGroup "simple" [ prop_empty_id, prop_mempty_id, prop_char
                                , prop_text, prop_line, prop_double ]
           , testGroup "complex" [ prop_hcat, prop_punctuate, prop_punctuate' ]
           ]

prop_empty_id :: TestTree
prop_empty_id = testGroup "empty"
                [ QC.testProperty "prop_empty_id" $
                  \(x :: Doc) -> empty <> x == x && x <> empty == x ]

prop_mempty_id :: TestTree
prop_mempty_id = testGroup "empty"
                 [ QC.testProperty "prop_mempty_id" $
                 \(x :: Doc) -> mempty `mappend` x == x && x `mappend` mempty == x ]

prop_char :: TestTree
prop_char = testGroup "char"
            [ QC.testProperty "prop_char" $ \(c :: Char) -> char c == Char c ]

prop_text :: TestTree
prop_text = testGroup "text"
            [ QC.testProperty "prop_text" $
              \(s :: String) -> text s == if null s then Empty else Text s ]

prop_line :: TestTree
prop_line = testGroup "line" [ QC.testProperty "prop_line" $ line == Line ]

prop_double :: TestTree
prop_double = testGroup "double"
              [ QC.testProperty "prop_double" $
                \(d :: Double) -> double d == text (show d) ]

prop_hcat :: TestTree
prop_hcat = testGroup "hcat"
            [ QC.testProperty "prop_hcat" $
              \(xs :: [Doc]) -> hcat xs == glue xs ]
    where glue []     = empty
          glue (d:ds) = d <> glue ds

prop_punctuate :: TestTree
prop_punctuate = testGroup "punctuate"
                 [ QC.testProperty "prop_punctuate" $
                   \(s :: Doc, xs :: [Doc]) -> punctuate s xs == intersperse s xs
                 ]

prop_punctuate' :: TestTree
prop_punctuate' = testGroup "punctuate"
                  [ QC.testProperty "prop_punctuate'" $
                    \(s :: Doc, xs :: [Doc]) ->
                    punctuate s xs == combine (intersperse s xs) ]
    where combine []           = []
          combine [x]          = [x]
          combine (x:Empty:ys) = x : combine ys
          combine (Empty:y:ys) = y : combine ys
          combine (x:y:ys)     = x `Concat` y : combine ys
