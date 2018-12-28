{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

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

data ParseResult a where
  RBool :: Bool -> ParseResult Bool
  RString :: String -> ParseResult String
  RInt :: Int -> ParseResult Int

unResult :: ParseResult a -> a
unResult (RBool a)   = a
unResult (RString a) = a
unResult (RInt a)    = a

data Nmis = Nmis
  { active :: Maybe Bool
  , authkey :: Maybe String
  , authpassword :: Maybe String
  , authprotocol :: Maybe String
  , businessService :: Maybe String
  , calls :: Maybe Bool
  , cbqos :: Maybe String
  , collect :: Maybe Bool
  , community :: Maybe Bool
  , context :: Maybe String
  , customer :: Maybe String
  , depend :: Maybe String
  , display_name :: Maybe String
  , group :: Maybe String
  , host :: Maybe String
  , location :: Maybe String
  , max_msg_size :: Maybe Int
  , max_repetitions :: Maybe Int
  , model :: Maybe String
  , name :: Maybe String
  , netType :: Maybe String
  , notes :: Maybe String
  , ping :: Maybe Bool
  , port :: Maybe Int
  , privkey :: Maybe String
  , privpassword :: Maybe String
  , privprotocol :: Maybe String
  , remote_connection_name :: Maybe String
  , remote_connection_url :: Maybe String
  , roleType :: Maybe String
  , serviceStatus :: Maybe String
  , services :: Maybe String
  , threshold :: Maybe Bool
  , timezone :: Maybe Int
  , username :: Maybe String
  , uuid :: Maybe String
  , version :: Maybe String
  , webserver :: Maybe Bool
  , wmipassword :: Maybe String
  , wmiusername :: Maybe String
  } deriving Show

