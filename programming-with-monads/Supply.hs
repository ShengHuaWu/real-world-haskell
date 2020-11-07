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
runSupply (S state) xs = runState state xs -- runState :: State s a -> s -> (a, s)

next :: Supply s (Maybe s)
next = S $ do
            state <- get                  -- get :: MonadState s m => m s
            case state of
              []     -> return Nothing
              (x:xs) -> do 
                          put xs          -- put :: MonadState s m => s -> m ()
                          return (Just x)

-- The followings are necessary if the `GeneralizedNewtypeDeriving` extension is not defined
-- unwrapS :: Supply s a -> State [s] a
-- unwrapS (S s) = s

-- instance Monad (Supply s) where
--     return  = S . return
--     s >>= f = S (unwrapS s >>= unwrapS . f)
