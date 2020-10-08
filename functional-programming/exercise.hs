module Exercise (splitLines, isLineTerminator) where
import Data.Char

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

-- Use `length` to check nullability is NOT a good idea in Haskell
-- because it will walk through the entire list
myDumbExample :: String -> Char
myDumbExample xs = if length xs > 0
                   then head xs
                   else 'Z'

myOtherExample :: String -> Char   
myOtherExample [] = 'Z'
myOtherExample (x:_) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

-- It takes a predicate and a list of any type, and splits its input list on every element for which the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p ls@(x:xs) -- as-pattern and it means bind the variable `ls` to the value that matches the right side of the `@` symbol
    | p x = [takeWhile p ls] ++ splitWith p (dropWhile p ls)
    | otherwise = splitWith p xs

-- `foldl (+) 0 [1..1000000]` will cause `Exception: stack overflow`
-- `foldl' (+) 0 [1..1000000]` will evaluate the result: 500000500000 (:module +Data.List) in ghci
-- The reason is `foldl` will create lots of thunks (memory footprints)

asIntStep :: Int -> Char -> Int
asIntStep result next = result * 10 + digitToInt next  

asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold ('-':xs) = - asInt_fold xs
asInt_fold xs = foldl asIntStep 0 xs

myConcat :: [[a]] -> [a]
myConcat = foldr (\next result -> next ++ result) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p xs = foldr step [] xs
    where step next result | p next = next:result | otherwise = []
