{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns    #-}
module Nat where
-- Permutation sort with Peano numbers and user-defined lists

data Nat = O | S Nat
  deriving Eq

instance Enum Nat where
  succ = S
  pred (S n) = n
  pred _     = failed

  fromEnum O = 0
  fromEnum (S n) = fromEnum n + 1

  toEnum n | n == 0    = O
           | n >  0    = S (toEnum (n-1))
           | otherwise = failed
