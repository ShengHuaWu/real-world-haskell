module PutJSON 
    (
        renderJValue
    ,   putJValue
    ) where

import Data.List (intercalate)
import SimpleJSON

renderPairs :: [(String, JValue)] -> String
renderPairs []    = ""
renderPairs pairs = intercalate ", " (map renderPair pairs)

renderPair :: (String, JValue) -> String
renderPair (key, value) = show key ++ ": " ++ renderJValue value

renderValue :: [JValue] -> String
renderValue []     = ""
renderValue values = intercalate ", " (map renderJValue values)

renderJValue :: JValue -> String
renderJValue (JString string) = show string
renderJValue (JNumber number) = show number
renderJValue (JBool True)     = "true"
renderJValue (JBool False)    = "false"
renderJValue JNull            = "null"
renderJValue (JObject pairs) = "{" ++ renderPairs pairs ++ "}"
renderJValue (JArray value)   = "[" ++ renderValue value ++ "]"

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue
