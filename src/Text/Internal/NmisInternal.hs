{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Text.Internal.NmisInternal
Description : Internal module
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}
module Text.Internal.NmisInternal where

import Text.Internal.ParseCombinators
import Text.Internal.NmisTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import Universum hiding (try, group)

-- |  Parse nmis file to [Nmis]
--
--    __This is a single function you will need to parse the file__
parseNmis :: Parser [Nmis]
parseNmis =
  lexeme $ do
    void $ optional $ symbol "#"
    void $ phash
    void $ space >> string "("
    parseToList

-- | Parses single record
parseSingle :: Parser Nmis
parseSingle = do
  void $ optional pQuotedStr
  void $ until "=>" >> space >> string "{" >> newline
  active <- try pTrue <|> pFalse
  authkey <- pQuotedVal
  authpassword <- pQuotedVal
  authprotocol <- pQuotedVal
  businessService <- pQuotedVal
  calls <- try pTrue <|> pFalse
  cbqos <- pQuotedVal
  collect <- try pTrue <|> pFalse
  community <- pQuotedVal
  context <- pQuotedVal
  customer <- pQuotedVal
  depend <- pQuotedVal
  display_name <- pQuotedVal
  group <- pQuotedVal
  host <- pQuotedVal
  location <- pQuotedVal
  max_msg_size <- pMInt
  max_repetitions <- pMInt
  model <- pQuotedVal
  name <- pQuotedVal
  netType <- pQuotedVal
  notes <- pQuotedVal
  ping <- try pTrue <|> pFalse
  port <- pInt
  privkey <- pQuotedVal
  privpassword <- pQuotedVal
  privprotocol <- pQuotedVal
  remote_connection_name <- pQuotedVal
  remote_connection_url <- pQuotedVal
  roleType <- pQuotedVal
  serviceStatus <- pQuotedVal
  services <- pQuotedVal
  threshold <- try pTrue <|> pFalse
  timezone <- pInt
  username <- pQuotedVal
  uuid <- pQuotedVal
  version <- pQuotedVal
  webserver <- try pTrue <|> pFalse
  wmipassword <- pQuotedVal
  wmiusername <- pQuotedVal
  void $ optional space >> string "}"
  void $ optional $ string ","
  return Nmis {..}

-- | parses many single values until ');'
parseToList :: Parser [Nmis]
parseToList = do
  result <-
    manyTill parseSingle (try $ lookAhead $ optional space >> string ")")
  void $ optional newline
  void $ string ")"
  void $ optional $ string ";"
  return result

-- | Show maybe integer for timezone field
showMaybeInt :: Maybe Integer -> String
showMaybeInt i = case i of
  Nothing -> ""
  Just x -> show x
