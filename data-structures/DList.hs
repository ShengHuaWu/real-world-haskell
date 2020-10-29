module Dlist (
    DList
  , fromList
  , toList
  , empty
  , append
  , cons
  , dfoldr
) where

import Test.QuickCheck

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

-- Equivalent of the list type's (:) operator
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

-- `Dlist` is a Monoid
instance Semigroup (DList a) where
  xs <> ys = append xs ys

instance Monoid (DList a) where
  mempty = empty
  mappend = append

-- Properties tests
-- Before running any properties test, `DList` has to conform `Show` & `Arbitrary`
instance (Show a) => Show (DList a) where
  show = show . toList

instance (Arbitrary a) => Arbitrary (DList a) where
  arbitrary = fromList <$> arbitrary -- The first `arbitrary` is `Gen (DList a)` and the second one is `Gen [a]`

prop_mappend_mempty :: Ord a => DList a -> Bool
prop_mappend_mempty xs = (toList $ mempty `mappend` xs) == (toList $ xs `mappend` mempty)
