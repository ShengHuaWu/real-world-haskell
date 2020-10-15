{-  Produce an effect when performed, but not when evaluated. That is, they only produce an effect when called by something else in an I/O context.
    Any expression may produce an action as its value, but the action will not perform I/O until it is executed inside another I/O action (or it is main).
    Performing (executing) an action of type IO t may perform I/O and will ultimately deliver a result of type t. -}

{-  There are three pre-defined Handles in System.IO. 
    They are `stdin`, which corresponds to standard input; `stdout` for standard output; and `stderr` for standard error. -}
import System.IO
import Data.Char(toUpper)

-- Convert lines to uppercase from `inHandle` and put them to `outHandle`
mainloop :: Handle -> Handle -> IO ()
mainloop inHandle outHandle = do 
    atEOF <- hIsEOF inHandle
    if atEOF
        then return ()
        else do line <- hGetLine inHandle
                hPutStrLn outHandle $ toUpper <$> line
                mainloop inHandle outHandle

-- Use `hGetContents` to obtain the text from the input file.
-- Must not close the `inHandle` until we have finished consuming its results via `hGetContents`.
mainloop1 :: Handle -> Handle -> IO ()
mainloop1 inHandle outHandle = do
    contents <- hGetContents inHandle -- The `contents` is evaluated lazily.
    hPutStrLn outHandle $ toUpper <$> contents

{-  Use `readFile` and `writeFile` to simplify the code.
    They will open and close the file handle under the hood. -}
readAndWrite :: IO ()
readAndWrite = do
    contents <- readFile "/Users/sw819i/Development/Private/real-world-haskell/input-and-output/input.txt"
    writeFile "/Users/sw819i/Development/Private/real-world-haskell/input-and-output/output.txt" $ toUpper <$> contents

{- `input.txt` should exist beforehand.
    Use absolute paths for both of `input.txt` and `output.txt`-}
main = do
    -- inHandle <- openFile "/Users/sw819i/Development/Private/real-world-haskell/input-and-output/input.txt" ReadMode
    -- outHandle <- openFile "/Users/sw819i/Development/Private/real-world-haskell/input-and-output/output.txt" WriteMode
    -- mainloop1 inHandle outHandle
    -- hClose inHandle
    -- hClose outHandle
    readAndWrite
