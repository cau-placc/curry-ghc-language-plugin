module SelectD where

import Plugin.CurryPlugin.Monad
import Plugin.CurryPlugin.BuiltIn (ListND)

{-# ANN module Nondeterministic #-}

fromToOneOneHundred :: [Int]
fromToOneOneHundred = [1..100]

fromToOneOneHundredP :: Curry (ListND Int)
fromToOneOneHundredP = liftE (return fromToOneOneHundred)
