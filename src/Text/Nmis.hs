{-|
Module      : Text.Nmis
Description : Main module for nmis parsing
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental

-}

--   NMIS stands for Network Management Information System
--
--   This parser parses the nmis format files to Nmis record type
--
--  Example usage :
--
--  module Main where
--
--
--  import Data.Either
--
--  import System.Environment (getArgs)
--
--  import System.IO
--
--  import Text.Megaparsec
--
--  import Text.Nmis
--
--  main :: IO ()
--
--  main = getArgs >>= parseArgs
--
--    where
--
--      parseArgs [] = putStrLn "error: you need to pass in the file path"
--
--      parseArgs (path:_) = do
--
--        contents <- readFile path
--
--        either (print . parseErrorPretty) print (parse parseNmis "" contents)
--
--

{-# LANGUAGE OverloadedStrings #-}

module Text.Nmis
  ( module Nmis
  ) where

import Text.Internal.NmisInternal as Nmis

