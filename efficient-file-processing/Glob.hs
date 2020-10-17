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

-- Match one given char
one :: Char -> String -> (Bool, String)
one c s@(x:xs) 
    | x == c    = (True, xs)
    | otherwise = (False, s)

-- Match any one or more chars until encountering a given char
oneOrMoreUntil :: Char -> String -> (Bool, String)
oneOrMoreUntil c s
    | result && x == c = (True, s)
    | result           = oneOrMoreUntil c xs 
    | otherwise        = (False, s) 
    where
        (result, (x:xs)) = Glob.any s


-- Match a text with a given pattern
match :: String -> String -> (Bool, String)
match [] []     = (True, [])
match [] _      = (False, [])
match input []  = (False, input)
match input@(t:ts) pattern@(p:ps)
    | p == '*' && ps == [] = (True, [])
    | p == '*' && ps /= [] = oneOrMoreUntil (head ps) input -- !!!: This is not correct
    | p == '?'  = Glob.any input -- !!!: This is not correct
    | t == p    = match ts ps
    | otherwise = (False, input)

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