{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
module Nat where
-- Permutation sort with Peano numbers and user-defined lists

data Nat = O | S Nat
