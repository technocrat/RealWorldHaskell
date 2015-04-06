module Chapter16.HttpRequestParser where

import           Control.Applicative           hiding (many, optional, (<|>))
import           Control.Monad                 (liftM4)
import           Text.ParserCombinators.Parsec

data Method = Get | Post deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest { reqMethod  :: Method
                               , reqURL     :: String
                               , reqHeaders :: [(String, String)]
                               , reqBody    :: Maybe String
                               } deriving (Eq, Show)

pRequest :: CharParser () HttpRequest
pRequest = q "GET" Get (pure Nothing) <|> q "POST" Post (Just <$> many anyChar)
  where q name ctor = liftM4 HttpRequest req url pHeaders
          where req = ctor <$ string name <* char ' '
        url = optional (char '/') *>
              manyTill notEOL (try $ string " HTTP/1." <* oneOf "01") <* crlf

pHeaders :: CharParser st [(String, String)]
pHeaders = header `manyTill` crlf
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        contents = liftA2 (++) (many1 notEOL <* crlf) (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
        fieldName = (:) <$> letter <*> many fieldChar
        fieldChar = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"
