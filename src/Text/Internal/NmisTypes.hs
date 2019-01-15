{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
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

type family Reducer (a :: Type) :: Type  where
  Reducer String = Nmis
  Reducer Text = Nmis
  Reducer ByteString = Nmis

data Red (a :: Type) :: Type where
  SRed :: String -> Red String
  BsRed :: ByteString -> Red ByteString
  TRed :: Text -> Red Text

reduce :: Red a -> Reducer a
reduce (SRed a) = undefined
reduce (BsRed a) = undefined
reduce (TRed a) = undefined

type Parser = Parsec Void String

data Nmis = Nmis
  { active :: !Bool
  , authkey :: !String
  , authpassword :: !String
  , authprotocol :: !String
  , businessService :: !String
  , calls :: !Bool
  , cbqos :: !String
  , collect :: !Bool
  , community :: !String
  , context :: !String
  , customer :: !String
  , depend :: !String
  , display_name :: !String
  , group :: !String
  , host :: !String
  , location :: !String
  , max_msg_size :: !(Maybe Int)
  , max_repetitions :: !(Maybe Int)
  , model :: !String
  , name :: !String
  , netType :: !String
  , notes :: !String
  , ping :: !Bool
  , port :: !Int
  , privkey :: !String
  , privpassword :: !String
  , privprotocol :: !String
  , rancid :: !Bool
  , remote_connection_name :: !String
  , remote_connection_url :: !String
  , roleType :: !String
  , serviceStatus :: !String
  , services :: !String
  , threshold :: !Bool
  , timezone :: !Int
  , username :: !String
  , uuid :: !String
  , version :: !String
  , webserver :: !Bool
  , wmipassword :: !String
  , wmiusername :: !String
  } deriving Show

