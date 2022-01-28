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

data MyBool = F | T

myNot :: MyBool -> MyBool
myNot T = F
myNot F = T

not2 :: MyBool -> MyBool
not2 x = myNot (myNot x)