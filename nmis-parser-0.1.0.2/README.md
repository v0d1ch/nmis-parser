# NMIS file parser
 - NMIS stands for Network Management Information System

This parser parses the NMIS format files to `Nmis` record type

#### Example usage :

```haskell
  module Main where
  import System.Environment (getArgs)
  import System.IO
  import Text.Megaparsec
  import Text.Nmis

main :: IO ()
main = getArgs >>= parseArgs
  where
    parseArgs [] = putStrLn "error: you need to pass in the file path"
    parseArgs (path:_) = do
      contents <- readFile path
      either (print . parseErrorPretty) print (parse parseNmis "" contents)


```