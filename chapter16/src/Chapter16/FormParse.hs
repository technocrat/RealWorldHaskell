module Chapter16.FormParse where

import           Control.Applicative           hiding (many, (<|>))
import           Control.Monad                 (liftM2)
import           Numeric                       (readHex)
import           Text.ParserCombinators.Parsec

pQuery :: CharParser () [(String, Maybe String)]
pQuery = pPair `sepBy` char '&'

pPair :: CharParser () (String, Maybe String)
pPair = do name <- many1 pChar
           value <- optionMaybe (char '=' >> many pChar)
           return (name, value)

pPairApp1 :: CharParser () (String, Maybe String)
pPairApp1 = liftM2 (,) (many1 pChar) (optionMaybe (char '=' >> many pChar))

aPair :: CharParser () (String, Maybe String)
aPair = liftA2 (,) (many1 aChar) (optionMaybe (char '=' *> many aChar))

pChar :: CharParser () Char
pChar = oneOf urlBaseChars <|> (char '+' >> return ' ') <|> pHex

aChar :: CharParser () Char
aChar = oneOf urlBaseChars <|> (' ' <$ char '+') <|> aHex

urlBaseChars :: String
urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

pHex :: CharParser () Char
pHex = do _ <- char '%'
          a <- hexDigit
          b <- hexDigit
          let ((d, _):_) = readHex [a, b]
          return . toEnum $ d

aHex :: CharParser () Char
aHex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]
