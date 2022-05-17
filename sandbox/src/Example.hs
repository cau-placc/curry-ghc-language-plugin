{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Example where

import Plugin.CurryPlugin.Prelude

permutations :: [a] -> [a]
permutations []     = []
permutations (x:xs) = insert x (permutations xs)
  where
    insert e []     = [e]
    insert e (y:ys) = (e:y:ys) ? (y : insert e ys)

id :: a -> a
id x = x
