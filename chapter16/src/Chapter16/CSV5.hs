module Chapter16.CSV5 where

import           Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` char ','

cell :: GenParser Char st String
cell = many (noneOf ",\n\r")

eol :: GenParser Char st Char
eol = do _ <- char '\n'
         char '\r' <|> return '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
