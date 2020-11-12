import Numeric
import Text.ParserCombinators.Parsec

-- Other characters must be encoded as a % character followed by two hexadecimal digits. 
p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ') -- Spaces are treated specially, using a + character. 
     <|> p_hex

-- `Maybe String` is used to distinguish between “no value” and “empty value”.
p_pair :: CharParser () (String, Maybe String)
p_pair = do
    name <- many1 p_char -- `many1` will fail if its parser never succeeds
    value <- optionMaybe (char '=' >> many p_char) -- If the parser fails, `optionMaybe` doesn't fail: it returns Nothing.
    return (name, value)

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'
