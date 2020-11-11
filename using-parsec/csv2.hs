import Text.ParserCombinators.Parsec

{-
    The `sepBy` function takes two functions as arguments: 
    the first function parses some sort of content, while the second function parses a separator. 
    `sepBy` starts by trying to parse content, then separators, and alternates back and forth until it can't parse a separator. 
    It returns a list of all the content that it was able to parse.

    The `endBy` function is similar to `sepBy`, but expects the very last item to be followed by the separator. 
    That is, it continues parsing until it can't parse any more content.
-}

csvFile = endBy line eol
line = sepBy cell (char ',') -- Every line must end with the end-of-line character
cell = many (noneOf ",\n") -- The last cell will not end with a comma
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input