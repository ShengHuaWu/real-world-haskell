module Dlist (
    DList
  , fromList
  , toList
  , empty
  , append
  , cons
  , dfoldr
) where

-- `unDL` :: DList a -> [a] -> [a]
newtype DList a = DL { unDL :: [a] -> [a] }

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++) -- (xs ++) :: [a] -> [a]

toList :: DList a -> [a]
toList (DL xs) = xs [] -- xs :: [a] -> [a]

empty :: DList a
empty = DL id

-- equivalent of the list type's (:) operator
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs) -- (x:) :: [a] -> [a] and xs :: [a] -> [a]

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f initial xs = foldr f initial (toList xs)

-- Its cost is linear in the number of appends we have performed to construct the `DList`.
safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
    (x:_) -> Just x
    _     -> Nothing

-- `DList` is a Functor
next :: (a -> b) -> a -> DList b -> DList b
next f x ys = f x `cons` ys

dmap :: (a -> b) -> DList a -> DList b
dmap f xs = fromList $ fmap f (toList xs)
-- dmap f = dfoldr (next f) empty

instance Functor DList where
    fmap = dmap
