{-# LANGUAGE OverloadedStrings #-}

module Text.Nmis (parseNmis) where

-- | NMIS stands for Network Management Interface Standard
--   This parser parses the nmis format files to Nmis record type


-- | Nmis file format looks something like this
-- -#
-- # THIS SOFTWARE IS NOT PART OF NMIS AND IS COPYRIGHTED, PROTECTED AND LICENSED
-- # ...
-- # *****************************************************************************
--
-- %hash = (
--   'some title' => {
--     'active' => 'true',
--     'authkey' => '',
--     'authpassword' => '',
--     'authprotocol' => 'md5',
--     'businessService' => '',
--     'calls' => 'false',
--     'cbqos' => 'none',
--     'collect' => 'true',
--     'community' => '',
--     'context' => '',
--     'customer' => '',
--     'depend' => 'N/A',
--     'display_name' => '',
--     'group' => '',
--     'host' => '00.000.00.000',
--     'location' => '',
--     'max_msg_size' => '',
--     'max_repetitions' => '',
--     'model' => 'automatic',
--     'name' => '',
--     'netType' => 'lan',
--     'notes' => '',
--     'ping' => 'true',
--     'port' => '',
--     'privkey' => '',
--     'privpassword' => '',
--     'privprotocol' => 'des',
--     'rancid' => 'false',
--     'remote_connection_name' => '',
--     'remote_connection_url' => '',
--     'roleType' => 'access',
--     'serviceStatus' => 'Production',
--     'services' => '',
--     'threshold' => 'true',
--     'timezone' => '0',
--     'username' => '',
--     'uuid' => '',
--     'version' => '',
--     'webserver' => 'false',
--     'wmipassword' => '',
--     'wmiusername' => ''
--   },
--   ...
-- );


-- | Example usage
-- module Main where
-- import System.Environment (getArgs)
-- import System.IO
-- import Text.Megaparsec
-- import Text.Nmis

-- -- | Example usage
-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [] -> putStrLn "error: you need to pass in file path"
--     [path] -> do
--             contents <- readFile path
--             let result = parse parseNmis "" contents
--             case result of
--                 Left nodes -> print $ parseErrorPretty nodes
--                 Right nodes -> do
--                     print nodes

--     _ -> putStrLn "error: you need to pass in only one file path"


import Control.Monad (join)
import qualified Data.Map.Strict as M
import Text.Helper as H
import Prelude hiding (until)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Read (readMaybe)
import Text.NmisTypes
import Data.Maybe (fromMaybe)


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


-- | Pass Nmiss and single Node to find a match
lookupInList :: [Nmis] ->  Nmis -> Nmis
lookupInList cust n = do
   let fcusts =  filterByGroup (group n) cust
   if length fcusts > 0 then
       n { customer = customer (head fcusts) }
   else n

-- | Filters Nmis by Nodes group
filterByGroup :: Maybe String -> [Nmis]  -> [Nmis]
filterByGroup s cl = filter (\c -> groups c == s) cl

-- | Parse nmis file to [Nmis]
parseNmis :: Parser [Nmis]
parseNmis =
  lexeme $ do
    _ <- optional $ symbol "#"
    _ <- phash
    _ <- space >> string "("
    parseToList

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

-- | Show Nmis
showNmis :: [Nmis] -> String
showNmis [] =  ""
showNmis (c:cx) =
  "\n  '" ++ fromMaybe "" (name c) ++ "' => {\n"
  ++ "    'customer' => '" ++ fromMaybe ""  (customer c) ++ "',\n"
  ++ "    'active' => '" ++ fromMaybe ""  (active c) ++ "',\n"
  ++ "    'authkey' => '" ++ fromMaybe ""  (authKey c) ++ "',\n"
  ++ "    'authpassword' => '" ++ fromMaybe ""  (authPassword c) ++ "',\n"
  ++ "    'authprotocol' => '" ++ fromMaybe ""  (authProtocol c) ++ "',\n"
  ++ "    'businesService' => '" ++ fromMaybe "" (businessService c) ++ "',\n"
  ++ "    'calls' => '" ++ fromMaybe ""  (calls c) ++ "',\n"
  ++ "    'cbqos' => '" ++ fromMaybe ""  (cbqos c) ++ "',\n"
  ++ "    'collect' => '" ++ fromMaybe ""  (collect c) ++ "',\n"
  ++ "    'community' => '" ++ fromMaybe ""  (community c) ++ "',\n"
  ++ "    'depend' => '" ++ fromMaybe ""  (depend c) ++ "',\n"
  ++ "    'display_name' => '" ++ fromMaybe "" (display_name c) ++ "',\n"
  ++ "    'group' => '" ++ fromMaybe ""  (group c) ++ "',\n"
  ++ "    'host' => '" ++ fromMaybe ""  (host c) ++ "',\n"
  ++ "    'location' => '" ++ fromMaybe ""  (location c) ++ "',\n"
  ++ "    'model' => '" ++ fromMaybe ""  (model c) ++ "',\n"
  ++ "    'name' => '" ++ fromMaybe ""  (name c) ++ "',\n"
  ++ "    'netType' => '" ++ fromMaybe ""  (netType c) ++ "',\n"
  ++ "    'ping' => '" ++ fromMaybe ""  (ping c) ++ "',\n"
  ++ "    'port' => '" ++ fromMaybe ""  (port c) ++ "',\n"
  ++ "    'rancid' => '" ++ fromMaybe ""  (rancid c) ++ "',\n"
  ++ "    'roleType' => '" ++ fromMaybe ""  (roleType c) ++ "',\n"
  ++ "    'serviceStatus' => '" ++ fromMaybe ""  (serviceStatus c) ++ "',\n"
  ++ "    'services' => '" ++ fromMaybe ""  (services c) ++ "',\n"
  ++ "    'threshold' => '" ++ fromMaybe ""  (threshold c) ++ "',\n"
  ++ "    'timezone' => '" ++ showMaybeInt (timezone c) ++ "',\n"
  ++ "    'uuid' => '" ++ fromMaybe ""  (uuid c) ++ "',\n"
  ++ "    'version' => '" ++ fromMaybe ""  (version c) ++ "',\n"
  ++ "    'webserver' => '" ++ fromMaybe ""  (webserver c) ++ "',\n"
  ++ "},"
  ++ showNmis cx
