module Chapter16.FormParse where

import           Control.Applicative           hiding (many, (<|>))
import           Control.Monad                 (liftM2)
import           Numeric                       (readHex)
import           Text.ParserCombinators.Parsec

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'


p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_pair_app1 :: CharParser () (String, Maybe String)
p_pair_app1 = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars       <|>
         (char '+' >> return ' ') <|>
         p_hex

a_char :: CharParser () Char
a_char = oneOf urlBaseChars <|>
         (' ' <$ char '+')  <|>
         a_hex

urlBaseChars :: String
urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  _ <- char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a, b]
  return . toEnum $ d

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]
