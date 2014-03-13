module Main where

import           Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: GenParser Char st String
quotedCell = do
  _ <- char '"'
  content <- many quotedChar
  _ <- char '"' <?> "quote at the end of cell"
  return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol :: GenParser Char st String
eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n"         <|>
      string "\r"         <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

main :: IO ()
main = do
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e  -> do putStrLn "Error parsing input: "
                  print e
    Right r -> mapM_ print r
