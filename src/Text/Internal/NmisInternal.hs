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
  -- _ <- liftIO $ print node
  -- let node = undefined
  void $ optional space >> string "}"
  void $ optional $ string ","
  let rmap = M.fromList node
  return
    Nmis
    { customer = unResult <$> M.lookup "customer" rmap
    , active = unResult <$> M.lookup "active" rmap
    , businessService = unResult <$> M.lookup "businessService" rmap
    , calls = unResult <$> M.lookup "calls" rmap
    , cbqos = unResult <$> M.lookup "cbqos" rmap
    , collect = unResult <$> M.lookup "collect" rmap
    , community = unResult <$> M.lookup "community" rmap
    , depend = unResult <$> M.lookup "depend" rmap
    , display_name = unResult <$> M.lookup "display_name" rmap
    , group = unResult <$> M.lookup "group" rmap
    , host = unResult <$> M.lookup "host" rmap
    , location = unResult <$> M.lookup "location" rmap
    , model = unResult <$> M.lookup "model" rmap
    , name = unResult <$> M.lookup "name" rmap
    , netType = unResult <$> M.lookup "netType" rmap
    , ping = unResult <$> M.lookup "ping" rmap
    , port = unResult <$> M.lookup "port" rmap
    , roleType = unResult <$> M.lookup "roleType" rmap
    , serviceStatus = unResult <$> M.lookup "serviceStatus" rmap
    , services = unResult <$> M.lookup "services" rmap
    , threshold = unResult <$> M.lookup "threshold" rmap
    , timezone = join $ readMaybe <$> unResult <$> M.lookup "timezone" rmap
    , uuid = unResult <$> M.lookup "uuid" rmap
    , version = unResult <$> M.lookup "version" rmap
    , webserver = unResult <$> M.lookup "webserver" rmap
    , authkey = unResult <$> M.lookup "authkey" rmap
    , authpassword = unResult <$> M.lookup "authpassword" rmap
    , authprotocol = unResult <$> M.lookup "authprotocol" rmap
    , context = unResult <$> M.lookup "authprotocol" rmap
    , max_msg_size = unResult <$> M.lookup "max_msg_size" rmap
    , max_repetitions = unResult <$> M.lookup "max_repetitions" rmap
    , notes = unResult <$> M.lookup "notes" rmap
    , privkey = unResult <$> M.lookup "privkey" rmap
    , privpassword = unResult <$> M.lookup "privpassword" rmap
    , privprotocol = unResult <$> M.lookup "privprotocol" rmap
    , remote_connection_name = unResult <$> M.lookup "remote_connection_name" rmap
    , remote_connection_url = unResult <$> M.lookup "remote_connection_url" rmap
    , username = unResult <$> M.lookup "username" rmap
    , wmipassword = unResult <$> M.lookup "wmipassword" rmap
    , wmiusername = unResult <$> M.lookup "wmiusername" rmap
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
