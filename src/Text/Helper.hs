{-# LANGUAGE OverloadedStrings #-}

module Text.Helper where

import Control.Applicative (empty)
import Control.Monad (void)
import Prelude hiding (until)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol scn

parrow :: Parser String
parrow = lexeme $ string " => "

pequals :: Parser String
pequals = lexeme $ string " = "

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

braces :: Parser a -> Parser a
braces = between (string "{") (string "}")

until :: String -> Parsec Dec String String
until s = anyChar `manyTill` string s

untilEol :: Parsec Dec String String
untilEol = anyChar `someTill` newline

phash :: Parser String
phash = lexeme $ string' "%hash" >> pequals

pQuotedStr :: Parser String
pQuotedStr = do
  _ <- optional space
  string' "'" >> until "'"

pUndefined :: Parser String
pUndefined = do
  _ <- optional space
  string' "undef"

pOpt :: Parser (String, String)
pOpt = do
  _ <- space
  key <- pQuotedStr
  _ <- space >> string "=>" >> space
  value <- try (pQuotedStr <|> pUndefined <|> many numberChar)
  _ <- optional $ string' ","
  _ <- newline
  return (key, value)
