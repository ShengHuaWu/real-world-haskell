import Test.QuickCheck
import Data.List

-- Quick sort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where 
        lhs = filter (<x) xs
        rhs = filter (>=x) xs

-- QuickCheck convention of prefixing test properties with `prop_` to distinguish them from normal code.
prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

{- 
    After sorting, the first element should be the smallest one.
    Filter the invalid input with `==>`, for example, here is the empty list.
    The return type has to be `Property` instead of `Bool`, 
    because it is now a function that filters non-empty lists, before testing them, rather than a simple boolean constant.
-}
prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

-- After sorting, the last element should be the largest one.
prop_maximum :: Ord a => [a] -> Property
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

{- 
    Append two lists together before sorting. 
    After sorting, the first element should be the smallest one of the lists.
-}
prop_append :: Ord a => [a] -> [a] -> Property
prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

-- After sorting, the output should be ordered.
ordered :: Ord a => [a] -> Bool
ordered []  = True
ordered [x] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_ordered :: Ord a => [a] -> Bool
prop_ordered = ordered . qsort

-- After sorting, the permutation should not change
permutation :: Ord a => [a] -> [a] -> Bool
permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_permutation :: Ord a => [a] -> Bool
prop_permutation xs = permutation xs (qsort xs)

-- Compare with the `sort` function in the standard list library. (aka model-based testing)
prop_sort_model :: Ord a => [a] -> Bool
prop_sort_model xs = sort xs == qsort xs
