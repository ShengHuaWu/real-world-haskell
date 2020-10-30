module Logger (
    Log
  , Logger
  , runLogger
  , record
) where

-- Type definitions
type Log = [String]
newtype Logger a = Logger { execLogger :: (a, Log) }

-- Evaluates a logged action.
runLogger :: Logger a -> (a, Log)
runLogger = execLogger

-- Record something
record :: String -> Logger ()
record log = Logger ((), [log])

-- Implement custom `show`
instance (Show a) => Show (Logger a) where
    show = show . runLogger

-- Logger is Functor
instance Functor Logger where
    fmap f logger = Logger (f a, logs)
                    where
                        (a, logs) = runLogger logger

-- Logger is Applicative
instance Applicative Logger where
    pure a = Logger (a, [])
    loggerFunction <*> logger = Logger (f a, firstLogs ++ secondLogs)
                                where
                                    (a, firstLogs) = runLogger logger
                                    (f, secondLogs) = runLogger loggerFunction

-- Logger is Monad
instance Monad Logger where
    return       = pure
    logger >>= f = Logger (b, firstLogs ++ secondLogs)
                   where 
                        (a, firstLogs) = runLogger logger
                        (b, secondLogs) = runLogger $ f a
