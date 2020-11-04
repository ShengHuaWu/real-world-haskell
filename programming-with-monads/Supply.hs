{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- This language extension elimilates the boilerplate code for `Functor`, `Applicative`, and `Monad` instances

module Supply (
    Supply
  , next
  , runSupply
) where

import Control.Monad.State

{- 
    Type definition
    We can derive `Functor`, `Applicative`, and `Monad` instances directly 
    because of the `GeneralizedNewtypeDeriving` extension and `State` is already a `Monad`.
-}
newtype Supply s a = S (State [s] a) deriving (Functor, Applicative, Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply = undefined

next :: Supply s (Maybe s)
next = undefined

-- The followings are necessary if the `GeneralizedNewtypeDeriving` extension is not defined
-- unwrapS :: Supply s a -> State [s] a
-- unwrapS (S s) = s

-- instance Monad (Supply s) where
--     return  = S . return
--     s >>= f = S (unwrapS s >>= unwrapS . f)
