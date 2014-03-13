module Chapter16.CSV6 where

import           Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = many (noneOf ",\n\r")

eol :: GenParser Char st String
eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n"         <|>
      string "\r"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"
