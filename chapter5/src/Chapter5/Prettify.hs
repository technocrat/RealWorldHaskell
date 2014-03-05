module Chapter5.Prettify
    ( Doc
    , (<>)
    , text
    , double
    , char
    , hcat
    ) where

-- import Chapter5.SimpleJSON

data Doc = ToBeDefined
           deriving Show

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

text :: String -> Doc
text = undefined

double :: Double -> Doc
double = undefined

char :: Char -> Doc
char = undefined

hcat :: [Doc] -> Doc
hcat = undefined
