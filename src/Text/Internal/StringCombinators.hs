{-|
Module      : Text.Internal.Helper
Description : Helper for parse actions
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Internal.StringCombinators
  ( pQuotedVal
  , pTrue
  , pFalse
  , pInt
  , pMInt
  , pUndefined
  , pHash
  , symbol
  , lineStart
  )
  where

import Text.Internal.NmisTypes (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Universum

-- | space consumer - consume space and comments
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | hashbang line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- | space consumer no 2
scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

-- | lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | symbol
symbol :: String -> Parser String
symbol = L.symbol scn

-- | equals - parse equals sign
pequals :: Parser String
pequals = lexeme $ string " = "

-- | parse until string passed as function parameter
until :: String -> Parser String
until s = anySingle `manyTill` symbol s

-- | parse until newline character
pHash :: Parser String
pHash = lexeme $ string' "%hash" >> pequals

integer :: Parser Int
integer = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = symbol "'" *> manyTill L.charLiteral (symbol "'")

-- | parse quoted string with optional space in front of it
pQuotedStr :: Parser String
pQuotedStr = string' "'" >> until "'"

pQuotedVal :: Parser String
pQuotedVal = do
  lineStart
  str <- stringLiteral
  lineEnd
  return str

-- | parse maybe integer
pMInt :: Parser (Maybe Int)
pMInt = do
  lineStart
  void $ symbol "'"
  int <- optional integer
  void $ symbol "'"
  lineEnd
  return int

-- | parse integer
pInt :: Parser Int
pInt = do
  lineStart
  void $ symbol "'"
  int <- integer
  void $ symbol "'"
  lineEnd
  return int

-- | parse 'undef' literal
pUndefined :: Parser String
pUndefined = do
  lineStart
  void $ symbol "'"
  undef <- string' "undef"
  void $ symbol "'"
  lineEnd
  return undef

-- | parse 'true' literal
pTrue :: Parser Bool
pTrue = do
  lineStart
  void $ symbol "'"
  _ <- string' "true"
  void $ symbol "'"
  lineEnd
  return True

-- | parse 'false' literal
pFalse :: Parser Bool
pFalse = do
  lineStart
  void $ symbol "'"
  _ <- string' "false"
  void $ symbol "'"
  lineEnd
  return False

lineStart :: Parser ()
lineStart = do
  void $ optional space
  _ <- pQuotedStr
  void $ optional space
  void $ string "=>"
  void $ optional space

lineEnd :: Parser ()
lineEnd = do
  void $ optional $ symbol ","
  void $ optional newline
