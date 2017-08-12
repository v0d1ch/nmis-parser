{-|
Module      : Text.Internal.NmisTypes
Description : Contains single type that holds all parsed data  
Copyright   : (c) Sasa Bogicevic, 2017
License     : GPL-3
Maintainer  : brutallesale@gmail.com
Stability   : experimental
-}
module Text.Internal.NmisTypes where

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
