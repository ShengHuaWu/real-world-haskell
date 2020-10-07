import System.Environment (getArgs)
import Exercise

fixLines :: String -> String
fixLines input = unlines $ splitLines input

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below, but the type should be `String -> String`
        myFunction = fixLines