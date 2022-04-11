module Plugin.ParsePlugin (plugin) where

import GHC.Plugins

import Plugin.LanguagePlugin
import Plugin.Trans.Config

plugin :: Plugin
plugin = setConfigFlagsFor flags languagePlugin
  where
    flags = [ (fst monadModConfigStr, "Plugin.ParsePlugin.Monad")
            , (fst monadNameConfigStr, "Parse")
            , (fst funModConfigStr, "Plugin.ParsePlugin.Monad")
            , (fst funNameConfigStr, "-->")
            , (fst preludeModConfigStr, "Plugin.ParsePlugin.Prelude")
            , (fst builtInModConfigStr, "Plugin.ParsePlugin.BuiltIn") ]
