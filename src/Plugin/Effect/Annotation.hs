{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-|
Module      : Plugin.Effect.Annotation
Description : Effect annotation used by the plugin
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains the data type that is used by the plugin to mark
plugin-compiled modules.
-}
module Plugin.Effect.Annotation (EffectTag(..)) where

import Data.Data

import GHC.Utils.Outputable

-- | This data type is used to tag plugin-compiled modules.
data EffectTag = ParseEffect
  deriving (Eq, Data)

instance Outputable EffectTag where
  ppr _ = "ParseEffect"
