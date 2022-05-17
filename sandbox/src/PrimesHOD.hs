module PrimesHOD where
-- Computing prime numbers via sieve and primitive higher-order functions

import Plugin.CurryPlugin.Monad

{-# ANN module Nondeterministic #-}

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = myfilter (isdivs n) ns
the_filter _      = error ""

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
myhead _       = error ""

at :: [Int] -> Int -> Int
at (x:xs) n = if n==0  then x
                       else at xs (n - 1)
at _      _ = error ""

primesHO_1000, primesHO_2000 :: Int
primesHO_1000 = at primes 1000
primesHO_2000 = at primes 2000

main :: Int
main = primesHO_2000

mainP :: Curry Int
mainP = liftE $ return main
