{- 
    Type definition:
    Take a state `s`, do something with it, and return a result `a` and possibly a new state `s`. 
-}
type SimpleState s a = s -> (a, s)
{-
    We cannot make `SimpleState` as an instance of `Functor` because it's just a type synomyn, 
    which has to be fully applied al all times. Therefore, using `newType` is a better way.
-} 

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

-- step -> makeStep -> newStep
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt sa f = \s -> let (a, s1) = sa s
                        sb      = f a
                    in sb s1

-- Takes the current state and returns it as the result
getSt :: SimpleState s s
getSt = \s -> (s, s)

-- Ignores the current state and replaces it with a new state
putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
