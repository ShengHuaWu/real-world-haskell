{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (IOException(..), bracket, handle)
import System.IO (IOMode(..), Handle(..), hClose, hFileSize, openFile)
import RecursiveContents

-- Find the file paths filtered by a predicate in one directory 
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind predicate dirPath = filter predicate <$> getRecursiveContents dirPath

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

{- 
    Only care about `IOException` here. 
    This signature helps ghc to figure out the type inference when using `handle nothing $ ...`.
-}
nothing :: IOException -> IO (Maybe a)
nothing _ = return Nothing

-- Get the size from a file (This is unsafe)
getSize :: Handle -> IO (Maybe Integer)
getSize handle = Just <$> hFileSize handle

-- Safely get the size of a file
getFileSize :: FilePath -> IO (Maybe Integer)
{-
    handle :: Exception e => (e -> IO a) -> IO a -> IO a

    bracket :: IO a    -- computation to run first ("acquire resource")
        -> (a -> IO b) -- computation to run last ("release resource")
        -> (a -> IO c) -- computation to run in-between
        -> IO c
    It is similar to `try-catch-finally` in Java
-}
getFileSize filePath = handle nothing $ bracket (openFile filePath ReadMode) hClose $ getSize

-- Check the file path by the above defined `Predicate` type
check :: Predicate -> FilePath -> IO Bool
check predicate filePath = do
    permissions <- getPermissions filePath
    size        <- getFileSize filePath
    modifiedAt  <- getModificationTime filePath
    return (predicate filePath permissions size modifiedAt)

-- Find the file paths filtered by the above defined `Predicate` type in one directory
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind predicate path = getRecursiveContents path >>= filterM (check predicate)

main = do
    paths <- simpleFind (\filePath -> takeExtension filePath == ".swift" ) "/Users/sw819i/Downloads/Temp"
    print paths