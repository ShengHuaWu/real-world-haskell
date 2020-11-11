import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

{-
    `try` function applies a parser. If the parser doesn't succeed, `try` behaves as if it hadn't consumed any input at all. 
    So, when you use `try` on the left side of `<|>`, Parsec will try the option on the right even if the left side failed after consuming some input. 
    `try` only has an effect if it is on the left of a `<|>`.
-}

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    {-
        `fail "Couldn't fine EOL"` will produce:
        unexpected end of input
        expecting ",", "\n\r", "\r\n", "\n" or "\r"
        Couldn't find EOL
    -}
    -- <|> fail "Couldn't find EOL"
    {-
        `<?> "end of line"` will produce:
        unexpected end of input
        expecting "," or end of line
    -}
    <?> "end of line" -- `<?>` first tries the parser on its left. Instead of trying another parser in the event of a failure, it presents an error message.
    

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input