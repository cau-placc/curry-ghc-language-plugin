module SortPrimesD where
-- Benchmark to measure sharing across non-determinism

import Plugin.CurryPlugin.Monad
import Plugin.CurryPlugin.BuiltIn (ListND)

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

guard :: Bool -> a -> a
guard True x = x
guard _    _ = error ""

myand :: Bool -> Bool -> Bool
myand True y  = y
myand False _ = False

---------------
-- Insertion sort:
isort  :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert (isort xs)
 where
  insert []        = [x]
  insert zs@(y:ys) | x <= y    = x : zs
                   | otherwise = y : insert ys
  insert _         = error ""

---------------

primeList :: [Int]
primeList = [primes!!303, primes!!302, primes!!301, primes!!300]

primeListP :: Curry (ListND Int)
primeListP = liftE (return primeList)
