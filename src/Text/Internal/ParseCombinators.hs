{-|
Module      : Text.Internal.Helper
Description : Helper for parse actions
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Internal.ParseCombinators where

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

-- | arrow - parse haskell constraint like arrow sign
parrow :: Parser String
parrow = lexeme $ string " => "

-- | equals - parse equals sign
pequals :: Parser String
pequals = lexeme $ string " = "

-- | parse all between parens
parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

-- | parse all between braces
braces :: Parser a -> Parser a
braces = between (string "{") (string "}")

-- | parse until string passed as function parameter
until :: String -> Parser String
until s = anySingle `manyTill` string s

-- | parse until newline character
untilEol :: Parser String
untilEol = anySingle `someTill` newline

-- | parse until newline character
phash :: Parser String
phash = lexeme $ string' "%hash" >> pequals

integer :: Parser Int
integer = lexeme L.decimal

-- | parse quoted string with optional space in front of it
pQuotedStr :: Parser String
pQuotedStr = string' "'" >> until "'"

pQuotedVal :: Parser String
pQuotedVal = do
  lineStart
  str <- string' "'" >> until "'"
  lineEnd
  return str

-- | parse maybe integer
pMInt :: Parser (Maybe Int)
pMInt = do
  lineStart
  int <- optional integer
  lineEnd
  return int

-- | parse integer
pInt :: Parser Int
pInt = do
  lineStart
  int <- integer
  lineEnd
  return int

-- | parse 'undef' literal
pUndefined :: Parser (String, String)
pUndefined = do
  void $ optional space
  key <- pKey
  undef <- string' "undef"
  return (key, undef)

-- | parse 'true' literal
pTrue :: Parser Bool
pTrue = do
  lineStart
  _ <- string' "true"
  lineEnd
  return True

-- | parse 'false' literal
pFalse :: Parser Bool
pFalse = do
  lineStart
  _ <- string' "false"
  lineEnd
  return False

pKey :: Parser String
pKey = do
  void space
  key <- pQuotedStr
  void space
  void $ string "=>"
  void space
  return key

lineStart :: Parser ()
lineStart = do
  void pKey
  void $ optional space
  void $ symbol "'"

lineEnd :: Parser ()
lineEnd = do
  void $ symbol "'"
  void $ symbol ","
  void $ optional newline
