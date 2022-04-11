{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Plugin.Trans.Import
Description : A Renamer plugin to check if imported modules are compatible
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module implements the Renamer plugin that checks if all imported
modules have been compiled with the plugin or marked as compatible.
-}
module Plugin.Trans.Import (processImportPlugin) where

import Prelude hiding ((<>))
import Data.Maybe
import Control.Monad

import GHC.Hs.Extension
import GHC.Hs.Decls
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Unit.Module.Imported
import GHC.Unit.Module.ModDetails
import GHC.Unit.Home.ModInfo
import GHC.Unit.External
import GHC.Builtin.Names
import GHC.Plugins

import Plugin.Effect.Annotation
import Plugin.Trans.Util

-- | A plugin that checks if all imported modules
-- have been compiled with the plugin or marked as compatible.
-- Will produce an error message otherwise.
processImportPlugin :: TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
processImportPlugin env grp = checkImports env >> return (env, grp)

data IfaceLoadResult = LoadSuccess Module
                     | LoadFail Module (Maybe SrcSpan)

-- | A plugin that checks if all imported modules
-- have been compiled with the plugin or marked as compatible.
-- Will produce an error message otherwise.
checkImports :: TcGblEnv -> TcM ()
checkImports env = do
  -- Get the list of all external and home pacckage modules.
  externalRef <- hsc_EPS <$> getTopEnv
  external <- readTcRef externalRef
  home <- hsc_HPT <$> getTopEnv
  -- Get the name of the current compilation unit/module.
  let unit = moduleUnit (tcg_semantic_mod env)
  -- Get the environment of all external annotations.
  let annEnvExt = eps_ann_env external
  -- Get the annotations for each imported module, except Data.Kind.
  -- Data.Kind is special and allowed in the Plugin
  let mostMdls = filter (not . isDataKind) $ allImportedMdls env
  let anns = map (uncurry (getAnnFor unit home annEnvExt)) mostMdls
  -- Check if the annotations for every module contain the correct marker
  let lds = map (uncurry3 classifyWithLoadResult) anns
  -- Create an error for each incorrect import
  mapM_ errorOnFailedLoad lds
  -- Fail if at least one error was recorded.
  failIfErrsM

isDataKind :: (Module, [ImportedBy]) -> Bool
isDataKind (Module u n, _) =
  mkModuleName "Data.Kind" == n && u == baseUnit

-- | Get any 'EffectTag' module annotations for a given module
-- and the source span of the import declaration, if available.
getAnnFor :: Unit -> HomePackageTable -> AnnEnv -> Module -> [ImportedBy]
          -> (Module, [EffectTag], Maybe SrcSpan)
getAnnFor unit modinfo annsExt mdl imps
  | unit == moduleUnit mdl = case lookupHpt modinfo (moduleName mdl) of
      Nothing   -> panicAnyUnsafe "Cannot find info for module" mdl
      Just info -> (mdl, findAnns' (mkAnnEnv (md_anns (hm_details info))), imp)
  | otherwise = (mdl, findAnns' annsExt, imp)
    where
      imp = msum (map importSpanMaybe imps)
      findAnns' anns = findAnns deserializeWithData anns (ModuleTarget mdl)

-- | Get all imported modules.
allImportedMdls :: TcGblEnv -> [(Module, [ImportedBy])]
allImportedMdls = moduleEnvToList . imp_mods . tcg_imports

-- | Get the source span of an import, if available.
importSpanMaybe :: ImportedBy -> Maybe SrcSpan
importSpanMaybe (ImportedByUser i) = Just (imv_span i)
importSpanMaybe ImportedBySystem   = Nothing

-- | Classify a module import as ok or failed.
-- If it is classified as failed, then the span of the import is added as well.
classifyWithLoadResult :: Module -> [EffectTag] -> Maybe SrcSpan
                       -> IfaceLoadResult
classifyWithLoadResult mdl anns mspan =
  if notNull anns
    then LoadSuccess mdl
    else LoadFail mdl mspan

-- | Check if the given load result should provide an error message.
errorOnFailedLoad :: IfaceLoadResult -> TcM ()
errorOnFailedLoad (LoadSuccess _      ) = return ()
errorOnFailedLoad (LoadFail    mdl msp) =
  maybe id setSrcSpan msp $ addErrTc $
    "Error! Module" <+> quotes (ppr mdl) <> impType <>
    "was not compiled with the Curry-Plugin and cannot be imported."
  where
    impType
      | isNothing msp  = space <> parens "System import" <> space
      | mdl == pRELUDE = space <> parens "Possibly implicit import" <> space
      | otherwise      = space
