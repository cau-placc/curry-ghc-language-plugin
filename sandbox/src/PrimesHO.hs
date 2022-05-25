{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
module PrimesHO where
-- Computing prime numbers via sieve and primitive higher-order functions

import qualified PrimesHOD

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = myfilter (isdivs n) ns
the_filter _      = failed

primes :: [Int]
primes = mymap myhead (myiterate the_filter (myiterate suCC 2))

myfilter :: (Int -> Bool) -> [Int] -> [Int]
myfilter _ []     = []
myfilter p (x:xs) = if p x then x : myfilter p xs
                           else myfilter p xs

myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

mymap :: (a -> b) -> [a] -> [b]
mymap _ []     = []
mymap f (x:xs) = f x : mymap f xs


myhead :: [Int] -> Int
myhead (x : _) = x
myhead _       = failed

at :: [Int] -> Int -> Int
at (x:xs) n = if n==0  then x
                       else at xs (n - 1)
at _      _ = failed

primesHO_1000, primesHO_2000 :: Int
primesHO_1000 = at primes 1000
primesHO_2000 = at primes 2000

main :: Int
main = primesHO_2000

yesSharingAcrossND :: Int
yesSharingAcrossND =
  let p = at primes 799
  in p ? p

noSharingAcrossND :: Int
noSharingAcrossND =
  at primes 799 ? at primes 799

mainD :: Int
mainD = PrimesHOD.mainP

yesSharingAcrossNDD :: Int
yesSharingAcrossNDD =
  let p = at PrimesHOD.primesP 799
  in p ? p

noSharingAcrossNDD :: Int
noSharingAcrossNDD =
  at PrimesHOD.primesP 799 ? at PrimesHOD.primesP 799
