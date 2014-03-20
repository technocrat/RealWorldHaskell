module Chapter11.Prettify2 where

import           Control.Monad   (liftM, liftM2)
import           Data.Monoid     hiding ((<>))
import           Test.QuickCheck

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> y     = y
x     <> Empty = x
x     <> y     = x `Concat` y

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s  = Text s

line :: Doc
line = Line

double :: Double -> Doc
double = text . show

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

instance Monoid Doc where
    mempty = empty
    mappend = (<>)

{-
instance Arbitrary Doc where
    arbitrary = do
      n <- choose (1,6) :: Gen Int
      case n of 1 -> return Empty
                2 -> do x <- arbitrary
                        return (Char x)
                3 -> do x <- arbitrary
                        return (Text x)
                4 -> return Line
                5 -> do x <- arbitrary
                        y <- arbitrary
                        return (Concat x y)
                6 -> do x <- arbitrary
                        y <- arbitrary
                        return (Union x y)
-}

instance Arbitrary Doc where
    arbitrary = oneof [ return Empty
                      , liftM  Char   arbitrary
                      , liftM  Text   arbitrary
                      , return Line
                      , liftM2 Concat arbitrary arbitrary
                      , liftM2 Union  arbitrary arbitrary
                      ]
