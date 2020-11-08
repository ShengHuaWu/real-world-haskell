{-
    Let's say that we would like to guarantee to ourselves that a piece of code can read and write files on the local filesystem, 
    but that it will not access the network. We can't use the plain IO monad, because it won't restrict us.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO (
    HandleIO
  , Handle
  , IOMode(..)
  , runHandleIO
  , openFile
  , hClose
  , hPutStrLn
) where

import Control.Monad.Trans (MonadIO(..))
import System.IO (Handle, IOMode(..))
import qualified System.IO -- `qualified` means we can only use the interfaces with the prefix of the module, for example, `System.IO.hClose` instead of `hClose`
import System.Directory (removeFile)

{- 
    This is a technique to wrap an `IO` action in another monad, which is similar to the following in Swift.
    ```
    struct HandleIO<A> {
        let run: ((A) -> Void) -> Void
    }
    ```
    The disadvantage of wrapping `IO` in another monad is that we're still tied to a concrete implementation. 
    If we want to swap `HandleIO` for some other monad, we must change the type of every action that uses `HandleIO`.
-}
newtype HandleIO a = HandleIO { runHandleIO :: IO a } deriving (Functor, Applicative, Monad)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

--`liftIO` lets us embed an `IO` action in another monad.
instance MonadIO HandleIO where
    liftIO = HandleIO

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)
