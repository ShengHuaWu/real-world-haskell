-- https://en.wikipedia.org/wiki/Glob_(programming)
module Glob 
        (
            match
        ,   testCases
        ) where

type Text = String
type Pattern = String

-- Match a text with a given pattern
match :: Text -> Pattern -> Bool
match [] [] = True
match [] _  = False
match _ []  = False
match (t:ts) (p:ps)
    | p == '*'  = True && match ts ps -- !!!: This doesn't work
    | p == '?'  = match ts ps 
    | t == p    = match ts ps
    | otherwise = False

-- Tests
testCases :: IO ()
testCases = do
    print $ match "abc" "abc"
    print $ match "abc" "cba"
    print $ match "abc" "?bc"
    print $ match "bc" "?bc"
    print $ match "abc" "a*"
    print $ match "b" "b*"
    print $ match "abc" "*b*"
    print $ match "ab" "*b*"