{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Plugin.CurryPlugin.Encapsulation
Description : Encapsulation of Curryerminism
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains functions to encapsulate the Curryerminism of
plugin-compiled functions.
-}
module Plugin.CurryPlugin.Encapsulation
  ( Curry, SearchMode(..)
  , evalGeneric, evalN, eval, eval1, eval2
  ) where

import Plugin.CurryPlugin.THEval
import Plugin.CurryPlugin.Monad

-- | Evaluate a nullary nondeterministic function
-- with the given search strategy.
eval :: Normalform Curry a b
     => SearchMode -> Curry a -> [b]
eval = $(evalN 0)

-- | Evaluate a unary Curryerministic function
-- with the given search strategy and arguments
eval1 :: (Normalform Curry a1 a2, Normalform Curry b1 b2)
      => SearchMode -> Curry (a1 --> b1) -> a2 -> [b2]
eval1 = $(evalN 1)

-- | Evaluate a 2-ary Curryerministic function
-- with the given search strategy and arguments
eval2 :: ( Normalform Curry a1 a2
         , Normalform Curry b1 b2
         , Normalform Curry c1 c2)
      => SearchMode -> Curry (a1 --> b1 --> c1) -> a2 -> b2 -> [c2]
eval2 = $(evalN 2)
