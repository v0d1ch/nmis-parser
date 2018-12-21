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
    _ <- optional $ symbol "#"
    _ <- phash
    _ <- space >> string "("
    parseToList


-- | Parses single record
parseSingle :: Parser Nmis
parseSingle = do
  _ <- optional H.pQuotedStr
  _ <- until "=>" >> space >> string "{" >> newline
  node <- manyTill pOpt (try $ lookAhead $ optional space *> string "}")
  _ <- optional space >> string "}"
  _ <- optional $ string ","
  let rmap = M.fromList node
  return
    Nmis
    { customer = M.lookup "customer" rmap
    , groups = M.lookup "groups" rmap
    , active = M.lookup "active" rmap
    , businessService = M.lookup "businessService" rmap
    , advancedOptions = M.lookup "advancedOptions" rmap
    , authKey = M.lookup "authKey" rmap
    , authPassword = M.lookup "authPassword" rmap
    , authProtocol = M.lookup "authProtocol" rmap
    , calls = M.lookup "calls" rmap
    , cbqos = M.lookup "cbqos" rmap
    , collect = M.lookup "collect" rmap
    , community = M.lookup "community" rmap
    , depend = M.lookup "depend" rmap
    , display_name = M.lookup "display_name" rmap
    , group = M.lookup "group" rmap
    , host = M.lookup "host" rmap
    , location = M.lookup "location" rmap
    , model = M.lookup "model" rmap
    , name = M.lookup "name" rmap
    , netType = M.lookup "netType" rmap
    , ping = M.lookup "ping" rmap
    , port = M.lookup "port" rmap
    , rancid = M.lookup "rancid" rmap
    , roleType = M.lookup "roleType" rmap
    , serviceStatus = M.lookup "serviceStatus" rmap
    , services = M.lookup "services" rmap
    , threshold = M.lookup "threshold" rmap
    , timezone = join $ readMaybe <$> M.lookup "timezone" rmap
    , uuid = M.lookup "uuid" rmap
    , version = M.lookup "version" rmap
    , webserver = M.lookup "webserver" rmap
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
filterByGroup :: Maybe String -> [Nmis]  -> [Nmis]
filterByGroup s cl = filter (\c -> groups c == s) cl

-- | parses many single values until ');'
parseToList :: Parser [Nmis]
parseToList = do
  result <-
    manyTill parseSingle (try $ lookAhead $ optional space >> string ")")
  _ <- optional newline
  _ <- string ")"
  _ <- optional $ string ";"
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