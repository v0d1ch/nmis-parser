{-|
Module      : Text.Internal.Helper
Description : Helper for parse actions
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Internal.Helper where

import Text.Internal.NmisTypes (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Universum hiding (many, try)

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
pQuotedStr = do
  void $ optional space
  str <- string' "'" >> until "'"
  void lineEnd
  return str

pQuotedVal :: Parser String
pQuotedVal = do
  void pKey
  void $ optional space
  str <- string' "'" >> until "'"
  lineEnd
  return str

-- | parse integer
pInt :: Parser Int
pInt = do
  void pKey
  void $ optional space
  void $ single '\''
  int <- integer
  void $ single '\''
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
  void pKey
  void $ optional space
  void $ single '\''
  _ <- string' "true"
  void $ single '\''
  lineEnd
  return True

-- | parse 'false' literal
pFalse :: Parser Bool
pFalse = do
  void pKey
  void $ optional space
  void $ single '\''
  _ <- string' "false"
  void $ single '\''
  void lineEnd
  return False

lineEnd =
   void $ symbol "," >> optional newline

pKey = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  return key
