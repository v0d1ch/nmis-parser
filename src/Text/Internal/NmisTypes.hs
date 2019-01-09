
{-|
Module      : Text.Internal.NmisTypes
Description : Contains single type that holds all parsed data
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental
-}
module Text.Internal.NmisTypes where

import Text.Megaparsec
import Universum

type Parser = Parsec Void String

data Nmis = Nmis
  { active :: Bool
  , authkey :: String
  , authpassword :: String
  , authprotocol :: String
  , businessService :: String
  , calls :: Bool
  , cbqos :: String
  , collect :: Bool
  , community :: Bool
  , context :: String
  , customer :: String
  , depend :: String
  , display_name :: String
  , group :: String
  , host :: String
  , location :: String
  , max_msg_size :: Int
  , max_repetitions :: Int
  , model :: String
  , name :: String
  , netType :: String
  , notes :: String
  , ping :: Bool
  , port :: Int
  , privkey :: String
  , privpassword :: String
  , privprotocol :: String
  , remote_connection_name :: String
  , remote_connection_url :: String
  , roleType :: String
  , serviceStatus :: String
  , services :: String
  , threshold :: Bool
  , timezone :: Int
  , username :: String
  , uuid :: String
  , version :: String
  , webserver :: Bool
  , wmipassword :: String
  , wmiusername :: String
  } deriving Show

