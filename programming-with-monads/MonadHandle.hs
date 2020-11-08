-- `FunctionalDependencies` restricts the instance of a typeclass, which is similar to conditional conformance in Swift.
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module MonadHandle (MonadHandle(..)) where

import System.IO (IOMode(..))


{-
    The beauty of the typeclass approach is that we can swap one underlying monad for another without touching much code, 
    as most of our code doesn't know or care about the implementation. 
    For instance, we could replace `IO` with a monad that compresses files as it writes them out.
-}

-- `m -> h` (functional dependency) means for any instance of `MonadHandle`, there is exactly one handle type that we can use. 
class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath -> IOMode -> m h
    hPutStr :: h -> String -> m ()
    hClose :: h -> m ()
    hGetContents :: h -> m String

    hPutStrLn :: h -> String -> m ()
    hPutStrLn h s = hPutStr h s >> hPutStr h "\n"

-- See `MonadHandleIO.hs` to find the instance of `MonadHandle`
