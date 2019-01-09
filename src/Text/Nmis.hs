{-|
Module      : Text.Nmis
Description : Main module for nmis parsing
Copyright   : (c) Sasa Bogicevic, 2019
License     : GPL-3
Maintainer  : t4nt0r@pm.me
Stability   : experimental

-}

--   NMIS stands for Network Management Information System
--
--   This parser parses the nmis format files to Nmis record type

{-# LANGUAGE OverloadedStrings #-}

module Text.Nmis
  ( module Nmis
  ) where

import Text.Internal.NmisInternal as Nmis

