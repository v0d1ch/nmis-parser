{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
Module      : Text.Internal.NmisInternal
Description : Internal module
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}
module Text.Internal.NmisInternal where

import qualified Data.Binary                     as B
import           Data.ByteString.Lazy            hiding (group)
import qualified Data.Text                       as T
import           Text.Internal.NmisTypes
import           Text.Internal.StringCombinators
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Universum                       hiding (ByteString, group, try)

class HasNmis a where
  parseToNmis :: a -> Either (ParseErrorBundle String Void) [Nmis]

instance HasNmis String where
  parseToNmis str = parse parseNmisList "" str

instance HasNmis ByteString where
  parseToNmis bs = parse parseNmisList "" $ B.decode bs

instance HasNmis Text where
  parseToNmis txt = parse parseNmisList "" $ T.unpack txt

parseNmis :: HasNmis a => a -> Either (ParseErrorBundle String Void) [Nmis]
parseNmis a = parseToNmis a

-- |  Parse nmis file to [Nmis]
--
--    __This is a single function you will need to parse the file__
parseNmisList :: Parser [Nmis]
parseNmisList = do
  void $ optional $ symbol "#"
  void pHash
  void $ optional space >> string "(" >> optional newline
  void $ lineStart >> string "{" >> newline
  result <- manyTill parseSingle (char ')')
  void $ optional (string ";") >> optional newline
  return result

-- | Parses single record
parseSingle :: Parser Nmis
parseSingle = do
  active <- try pTrue <|> pFalse <?> "active"
  authkey <- pQuotedVal <?> " auth key"
  authpassword <- pQuotedVal <?> " auth password"
  authprotocol <- pQuotedVal <?> " auth protocol"
  businessService <- pQuotedVal <?> " bussiness service"
  calls <- try pTrue <|> pFalse <?> "calls"
  cbqos <- pQuotedVal <?> "cbqos"
  collect <- try pTrue <|> pFalse <?> "collect"
  community <- pQuotedVal <?> "community"
  context <- pQuotedVal <?> "context"
  customer <- pQuotedVal <?> "customer"
  depend <- pQuotedVal <?> "depend"
  display_name <- pQuotedVal <?> "display name"
  group <- pQuotedVal <?> "group"
  host <- pQuotedVal <?> "host"
  location <- pQuotedVal <?> "location"
  max_msg_size <- pMInt <?> "max message size"
  max_repetitions <- pMInt <?> "max repetitions"
  model <- pQuotedVal <?> "model"
  name <- pQuotedVal <?> "name"
  netType <- pQuotedVal <?> "netType"
  notes <- pQuotedVal <?> "notes"
  ping <- try pTrue <|> pFalse <?> "ping"
  port <- pInt <?> "port"
  privkey <- pQuotedVal <?> "privKey"
  privpassword <- pQuotedVal <?> "privpassword"
  privprotocol <- pQuotedVal <?> "privprotocol"
  rancid <- try pTrue <|> pFalse <?> "rancid"
  remote_connection_name <- pQuotedVal <?> "remote connection name"
  remote_connection_url <- pQuotedVal <?> "remote connection url"
  roleType <- pQuotedVal <?> "role type"
  serviceStatus <- pQuotedVal <?> "service status"
  services <- pQuotedVal <?> "services"
  threshold <- try pTrue <|> pFalse <?> "threshold"
  timezone <- pInt <?> " timezone"
  username <- pQuotedVal <?> "username"
  uuid <- pQuotedVal <?> "uuid"
  version <- pQuotedVal <?> "version"
  webserver <- try pTrue <|> pFalse <?> "webserver"
  wmipassword <- pQuotedVal <?> "wmipassword"
  wmiusername <- pQuotedVal <?> "wmiusername"
  void $ string "}" >> optional newline
  void $ optional $ string ","
  return Nmis {..}

