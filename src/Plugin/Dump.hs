{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Plugin.Dump
Description : Defines options and functions to dump intermediate output.
Copyright   : (c) Kai-Oliver Prott (2020 - 2023)
Maintainer  : kai.prott@hotmail.de

This module contains everything required to dump intermediate results
during the lifting performed by the plugin.
-}
module Plugin.Dump where

import Control.Monad ( foldM, when )

import GHC.Plugins
import GHC.Tc.Types

-- | A data type for a collection of options to dump intermediate results.
newtype DumpOpts = DOpts { d_phases :: [DumpPhase] }
  deriving (Semigroup, Monoid)

-- | An enumeration of all possibe options to dump intermediate results.
data DumpPhase = DumpOriginal | DumpOriginalEv
               | DumpOriginalInstEnv | DumpInstEnv
               | DumpOriginalTypeEnv
               | DumpPatternMatched
               | DumpDerivingErrs
  deriving (Eq, Show)

-- | Dump a given value to the output if the corresponding option flag is set.
dumpWith :: Outputable o => DumpPhase -> DumpOpts -> o -> TcM ()
dumpWith p opts o =
  when (p `elem` d_phases opts) (getDynFlags >>= liftIO . dump)
  where
    dump flgs = do
      let phaseStr = show p
      putStrLn phaseStr
      putStrLn (replicate (length phaseStr) '-')
      putStrLn (showPpr flgs o)

-- | All default options for dumping intermediate results.
defaultDumpOpts :: DumpOpts
defaultDumpOpts = DOpts []

-- | Try to parse command line options to collect
-- all options to dump intermediate results.
parseDumpOpts :: [CommandLineOption] -> Maybe DumpOpts
parseDumpOpts []   = Just defaultDumpOpts
parseDumpOpts opts = DOpts <$> foldM parseOne [] opts
  where
    parseOne xs "dump-original"           = Just (DumpOriginal       :xs)
    parseOne xs "dump-original-ev"        = Just (DumpOriginalEv     :xs)
    parseOne xs "dump-inst-env"           = Just (DumpInstEnv        :xs)
    parseOne xs "dump-original-inst-env"  = Just (DumpOriginalInstEnv:xs)
    parseOne xs "dump-original-type-env"  = Just (DumpOriginalTypeEnv:xs)
    parseOne xs "dump-pattern-matched"    = Just (DumpPatternMatched :xs)
    parseOne xs "dump-deriving-errs"      = Just (DumpDerivingErrs   :xs)
    parseOne xs "NoImplicitPrelude"       = Just xs
    parseOne _ _                          = Nothing
