module Chapter16.CSV7 where

import           Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` char ','

cell :: GenParser Char st String
cell = many (noneOf ",\n\r")

eol :: GenParser Char st String
eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <|> fail "Couldn't find EOL"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
