{-|
Module      : Text.Internal.NmisInternal
Description : Internal module
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}
module Text.Internal.NmisInternal where

import qualified Data.Map.Strict as M
import Text.Internal.Helper as H
import Text.Internal.NmisTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import Universum hiding (try)

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
  void $ optional H.pQuotedStr
  void $ until "=>" >> space >> string "{" >> newline
  -- node <- manyTill pOpt (try $ lookAhead $ optional space *> string "}")
  node <- sepBy1 (try $ pUndefined <|> pQuotedVal <|> pTrue <|> pFalse) (symbol ",")
  void $ optional space >> string "}"
  void $ optional $ string ","
  let rmap = M.fromList node
  return
    Nmis
    { customer = join $ unResult <$> M.lookup "customer" rmap
    , active = join $ unResult <$> M.lookup "active" rmap
    , businessService = join $ unResult <$> M.lookup "businessService" rmap
    , calls = join $ unResult <$> M.lookup "calls" rmap
    , cbqos = join $ unResult <$> M.lookup "cbqos" rmap
    , collect = join $ unResult <$> M.lookup "collect" rmap
    , community = join $ unResult <$> M.lookup "community" rmap
    , depend = join $ unResult <$> M.lookup "depend" rmap
    , display_name = join $ unResult <$> M.lookup "display_name" rmap
    , group = join $ unResult <$> M.lookup "group" rmap
    , host = join $ unResult <$> M.lookup "host" rmap
    , location = join $ unResult <$> M.lookup "location" rmap
    , model = join $ unResult <$> M.lookup "model" rmap
    , name = join $ unResult <$> M.lookup "name" rmap
    , netType = join $ unResult <$> M.lookup "netType" rmap
    , ping = join $ unResult <$> M.lookup "ping" rmap
    , port = join $ unResult <$> M.lookup "port" rmap
    , roleType = join $ unResult <$> M.lookup "roleType" rmap
    , serviceStatus = join $ unResult <$> M.lookup "serviceStatus" rmap
    , services = join $ unResult <$> M.lookup "services" rmap
    , threshold = join $ unResult <$> M.lookup "threshold" rmap
    , timezone = join $ unResult <$> M.lookup "timezone" rmap
    , uuid = join $ unResult <$> M.lookup "uuid" rmap
    , version = join $ unResult <$> M.lookup "version" rmap
    , webserver = join $ unResult <$> M.lookup "webserver" rmap
    , authkey = join $ unResult <$> M.lookup "authkey" rmap
    , authpassword = join $ unResult <$> M.lookup "authpassword" rmap
    , authprotocol = join $ unResult <$> M.lookup "authprotocol" rmap
    , context = join $ unResult <$> M.lookup "authprotocol" rmap
    , max_msg_size = join $ unResult <$> M.lookup "max_msg_size" rmap
    , max_repetitions = join $ unResult <$> M.lookup "max_repetitions" rmap
    , notes = join $ unResult <$> M.lookup "notes" rmap
    , privkey = join $ unResult <$> M.lookup "privkey" rmap
    , privpassword = join $ unResult <$> M.lookup "privpassword" rmap
    , privprotocol = join $ unResult <$> M.lookup "privprotocol" rmap
    , remote_connection_name = join $ unResult <$> M.lookup "remote_connection_name" rmap
    , remote_connection_url = join $ unResult <$> M.lookup "remote_connection_url" rmap
    , username = join $ unResult <$> M.lookup "username" rmap
    , wmipassword = join $ unResult <$> M.lookup "wmipassword" rmap
    , wmiusername = join $ unResult <$> M.lookup "wmiusername" rmap
    }


-- |
-- This function is used to match records by group/groups field.
--
-- You have two lists with Nmis types [Nmis] [Nmis]
--
-- and you want to match group field from one to the groups field from the other,
--
-- grab a customer field from first list and attach it to the second list which you can save later.
--
-- This is something of practical use which I needed in the real world.
-- lookupInList :: [Nmis] ->  Nmis -> Nmis
-- lookupInList cust n = do
--    let fcusts =  filterByGroup (group n) cust
--    if length fcusts > 0 then
--        n { customer = customer (head fcusts) }
--    else n

-- | Filters Nmis by Nodes group
-- filterByGroup :: Maybe String -> [Nmis]  -> [Nmis]
-- filterByGroup s cl = filter (\c -> groups c == s) cl

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

-- | Show Nmis list
--
-- Use for printing the [Nmis] list
-- showNmis :: [Nmis] -> String
-- showNmis [] =  ""
-- showNmis (c:cx) =
--   "\n  '" ++ fromMaybe "" (name c) ++ "' => {\n"
--   ++ "    'customer' => '" ++ fromMaybe ""  (customer c) ++ "',\n"
--   ++ "    'active' => '" ++ fromMaybe ""  (active c) ++ "',\n"
--   ++ "    'authkey' => '" ++ fromMaybe ""  (authKey c) ++ "',\n"
--   ++ "    'authpassword' => '" ++ fromMaybe ""  (authPassword c) ++ "',\n"
--   ++ "    'authprotocol' => '" ++ fromMaybe ""  (authProtocol c) ++ "',\n"
--   ++ "    'businesService' => '" ++ fromMaybe "" (businessService c) ++ "',\n"
--   ++ "    'calls' => '" ++ fromMaybe ""  (calls c) ++ "',\n"
--   ++ "    'cbqos' => '" ++ fromMaybe ""  (cbqos c) ++ "',\n"
--   ++ "    'collect' => '" ++ fromMaybe ""  (collect c) ++ "',\n"
--   ++ "    'community' => '" ++ fromMaybe ""  (community c) ++ "',\n"
--   ++ "    'depend' => '" ++ fromMaybe ""  (depend c) ++ "',\n"
--   ++ "    'display_name' => '" ++ fromMaybe "" (display_name c) ++ "',\n"
--   ++ "    'group' => '" ++ fromMaybe ""  (group c) ++ "',\n"
--   ++ "    'host' => '" ++ fromMaybe ""  (host c) ++ "',\n"
--   ++ "    'location' => '" ++ fromMaybe ""  (location c) ++ "',\n"
--   ++ "    'model' => '" ++ fromMaybe ""  (model c) ++ "',\n"
--   ++ "    'name' => '" ++ fromMaybe ""  (name c) ++ "',\n"
--   ++ "    'netType' => '" ++ fromMaybe ""  (netType c) ++ "',\n"
--   ++ "    'ping' => '" ++ fromMaybe ""  (ping c) ++ "',\n"
--   ++ "    'port' => '" ++ fromMaybe ""  (port c) ++ "',\n"
--   ++ "    'rancid' => '" ++ fromMaybe ""  (rancid c) ++ "',\n"
--   ++ "    'roleType' => '" ++ fromMaybe ""  (roleType c) ++ "',\n"
--   ++ "    'serviceStatus' => '" ++ fromMaybe ""  (serviceStatus c) ++ "',\n"
--   ++ "    'services' => '" ++ fromMaybe ""  (services c) ++ "',\n"
--   ++ "    'threshold' => '" ++ fromMaybe ""  (threshold c) ++ "',\n"
--   ++ "    'timezone' => '" ++ showMaybeInt (timezone c) ++ "',\n"
--   ++ "    'uuid' => '" ++ fromMaybe ""  (uuid c) ++ "',\n"
--   ++ "    'version' => '" ++ fromMaybe ""  (version c) ++ "',\n"
--   ++ "    'webserver' => '" ++ fromMaybe ""  (webserver c) ++ "',\n"
--   ++ "},"
--   ++ showNmis cx
