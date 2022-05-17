{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
module PermSortPeanoD where
-- Permutation sort with Peano numbers and user-defined lists

import Plugin.CurryPlugin.Monad
import Nat

{-# ANN module Nondeterministic #-}

data MyBool = MyTrue | MyFalse

ifThenElse :: MyBool -> a -> a -> a
ifThenElse MyTrue  x _ = x
ifThenElse MyFalse _ y = y

leq :: Nat -> Nat -> MyBool
leq O     _     = MyTrue
leq (S _) O     = MyFalse
leq (S x) (S y) = leq x y

isNat :: Nat -> MyBool
isNat O = MyTrue
isNat (S x) = isNat x

add :: Nat -> Nat -> Nat
add O n = n
add (S x) y = S (add x y)

double :: Nat -> Nat
double x = add x x

mult :: Nat -> Nat -> Nat
mult O _ = O
mult (S x) y = add y (mult x y)

two :: Nat
two = S (S O)

three :: Nat
three = S two

four :: Nat
four = double two

nat13 :: Nat
nat13 = S (mult three four)

nat13P :: Curry _
nat13P = liftE (return nat13)

nat14 :: Nat
nat14 = add two (mult three four)

nat15 :: Nat
nat15 = S nat14
