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

  main = do

    args <- getArgs

    case args of

      [] -> putStrLn "error: you need to pass in file path"

      [path] -> do

              contents <- readFile path

              let result = parse parseNmis "" contents

              case result of

                  Left nodes -> print $ parseErrorPretty nodes

                  Right nodes -> do

                      print nodes

      _ -> putStrLn "error: you need to pass in only one file path"

```
