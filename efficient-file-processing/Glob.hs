-- https://en.wikipedia.org/wiki/Glob_(programming)
module Glob 
        (
            match
        ,   testCases
        ) where

import Control.Applicative
import Data.Char

-- Match any one char
any :: String -> (Bool, String)
any []     = (False, [])
any (x:xs) = (True, xs)

-- Match zero or more chars until encountering a given char
zeroOrMoreUntil :: Char -> String -> (Bool, String)
zeroOrMoreUntil _ [] = (False, [])
zeroOrMoreUntil c input@(x:xs)
    | x == c    = (True, input)
    | otherwise = zeroOrMoreUntil c xs

-- Concat one match with another match
concat :: (String -> (Bool, String)) -> (String -> (Bool, String)) -> String -> (Bool, String)
concat match anotherMatch input
    | result    = anotherMatch rest
    | otherwise = (result, rest)
    where
        (result, rest) = match input

-- Match any one or more chars until encountering a given char
oneOrMoreUntil :: Char -> String -> (Bool, String)
oneOrMoreUntil c input = Glob.any `Glob.concat` (zeroOrMoreUntil c) $ input

-- Give a pattern to match a text
match :: String -> String -> (Bool, String)
match [] []     = (True, [])
match _ []      = (False, [])
match [] input  = (False, input)
match pattern@(p:ps) input@(t:ts) 
    | p == '*' && ps == [] = (True, [])
    | p == '*' && ps /= [] = oneOrMoreUntil (head ps) `Glob.concat` (match ps) $ input
    | p == '?'  = Glob.any `Glob.concat` (match ps) $ input
    | t == p    = match ps ts
    | otherwise = (False, input)

-- Tests
testCases :: IO ()
testCases = do
    print $ match "abc" "abc"
    print $ match "cba" "abc"
    print $ match "?bc" "abc" 
    print $ match "?bc" "bc" 
    print $ match "a*" "abc" 
    print $ match "b*" "b" 
    print $ match "*b" "ab" 
    print $ match "*b" "b"
    print $ match "*b*" "abc" 
    print $ match "*b*" "ab" 