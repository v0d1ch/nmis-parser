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

-- | Main type that holds all parsed data
data Nmis = Nmis
  { customer :: Maybe String
  , groups :: Maybe String
  , active :: Maybe String
  , advancedOptions :: Maybe String
  , authKey :: Maybe String
  , authPassword :: Maybe String
  , authProtocol :: Maybe String
  , businessService :: Maybe String
  , calls :: Maybe String
  , cbqos :: Maybe String
  , collect :: Maybe String
  , community :: Maybe String
  , depend :: Maybe String
  , display_name :: Maybe String
  , group :: Maybe String
  , host :: Maybe String
  , location :: Maybe String
  , model :: Maybe String
  , name :: Maybe String
  , netType :: Maybe String
  , ping :: Maybe String
  , port :: Maybe String
  , rancid :: Maybe String
  , roleType :: Maybe String
  , serviceStatus :: Maybe String
  , services :: Maybe String
  , threshold :: Maybe String
  , timezone :: Maybe Integer
  , uuid :: Maybe String
  , version :: Maybe String
  , webserver :: Maybe String
  } deriving (Show)

-- all fields
-- Nmis {
--     'active' => 'true',
--     'authkey' => '',
--     'authpassword' => '',
--     'authprotocol' => 'md5',
--     'businessService' => '',
--     'calls' => 'false',
--     'cbqos' => 'none',
--     'collect' => 'true',
--     'community' => 'public',
--     'context' => '',
--     'customer' => '',
--     'depend' => '',
--     'display_name' => '',
--     'group' => 'WIRELESS',
--     'host' => '10.12.255.140',
--     'location' => '2CX',
--     'max_msg_size' => '2800',
--     'max_repetitions' => '0',
--     'model' => 'automatic',
--     'name' => 'CA-Core-1',
--     'netType' => 'wan',
--     'notes' => 'Test Note ND080217',
--     'ping' => 'true',
--     'port' => '161',
--     'privkey' => '',
--     'privpassword' => '',
--     'privprotocol' => 'des',
--     'remote_connection_name' => 'SSH to Node',
--     'remote_connection_url' => 'ssh://$host',
--     'roleType' => 'core',
--     'serviceStatus' => 'Development',
--     'services' => '',
--     'threshold' => 'true',
--     'timezone' => '0',
--     'username' => '',
--     'uuid' => '40a860d3-afcc-11e6-b9b8-ad57ed8ffe33',
--     'version' => 'snmpv2c',
--     'webserver' => 'false',
--     'wmipassword' => '',
--     'wmiusername' => ''
--    }
