module Chapter5.Prettify
    ( Doc
    , (<>)
    , empty
    , char
    , text
    , line
    , double
    , fsep
    , hcat
    , punctuate
    , compact
    , pretty
    ) where

-- import Chapter5.SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s  = Text s

line :: Doc
line = Line

double :: Double -> Doc
double = text . show

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where transform []     = ""
          transform (d:ds) = case d of
                               Empty        -> transform ds
                               Char c       -> c : transform ds
                               Text s       -> s ++ transform ds
                               Line         -> '\n' : transform ds
                               a `Concat` b -> transform (a:b:ds)
                               _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = case d of
                              Empty        -> best col ds
                              Char c       -> c : best (col + 1) ds
                              Text s       -> s ++ best (col + length s) ds
                              Line         -> '\n' : best 0 ds
                              a `Concat` b -> best col (a:b:ds)
                              a `Union` b  -> nicest col (best col (a:ds))
                                                         (best col (b:ds))
          best _ _        = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
_ `fits` ""        = True
_ `fits` ('\n':_)  = True
w `fits` (_:cs)    = (w-1) `fits` cs
