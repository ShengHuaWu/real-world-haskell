-- http://book.realworldhaskell.org/read/types-and-functions.html

-- False :: Bool
-- (["foo", "bar"], 'a') :: ([[Char]], Char) 
-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

add a b = a + b

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne [_] = Nothing
lastButOne xs = Just $ last . take 2 . reverse $ xs
