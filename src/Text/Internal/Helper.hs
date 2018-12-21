{-# LANGUAGE PartialTypeSignatures #-}
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


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Internal.NmisTypes (Parser)
import Universum hiding (try, many)


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
until s = anyChar `manyTill` string s

-- | parse until newline character
untilEol :: Parser String
untilEol = anyChar `someTill` newline

-- | parse until newline character
phash :: Parser String
phash = lexeme $ string' "%hash" >> pequals

-- | parse quoted string with optional space in front of it
pQuotedStr :: Parser String
pQuotedStr = do
  _ <- optional space
  string' "'" >> until "'"

-- | parse 'undef' literal
pUndefined :: Parser String
pUndefined = do
  _ <- optional space
  string' "undef"

-- | combined parser for single record
pOpt :: Parser (String, String)
pOpt = do
  _ <- space
  key <- pQuotedStr
  _ <- space >> string "=>" >> space
  value <- try (pQuotedStr <|> pUndefined <|> many numberChar)
  _ <- optional $ string' ","
  _ <- newline
  return (key, value)

