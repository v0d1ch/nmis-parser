{-# LANGUAGE GADTs              #-}

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

import Text.Internal.NmisTypes (Parser, ParseResult (..))
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

integer       = lexeme L.decimal

-- | parse quoted string with optional space in front of it
pQuotedStr :: Parser String
pQuotedStr = do
  _ <- optional space
  string' "'" >> until "'"

pQuotedVal :: Parser (String, ParseResult)
pQuotedVal = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  void $ optional space
  str <- string' "'" >> until "'"
  return (key, RString str)

-- | parse integer
pInt :: Parser (String, ParseResult)
pInt = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  _ <- optional space
  int <- integer
  return (key, RInt int)

-- | parse 'undef' literal
pUndefined :: Parser (String, ParseResult)
pUndefined = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  _ <- optional space
  undef <- symbol "undef"
  return (key, RString undef)

-- | parse 'true' literal
pTrue :: Parser (String, ParseResult)
pTrue = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  void $ optional space
  _ <- symbol "true"
  return (key, RBool True)

-- | parse 'false' literal
pFalse :: Parser (String, ParseResult)
pFalse = do
  void $ space
  key <- pQuotedStr
  void $ space >> string "=>" >> space
  void $ optional space
  _ <- symbol "false"
  return (key, RBool False)

-- pInt :: Parser (Result a)
-- pInt = do
--   int <- many numberChar
--   return $ RInt int

-- | combined parser for single record
-- pOpt :: Parser (String, ParseResult)
-- pOpt = do
--   void $ space
--   key <- pQuotedStr
--   void $ space >> string "=>" >> space
--   value <- pTrue <|> pFalse <|> pUndefined
--   return (key, value)
