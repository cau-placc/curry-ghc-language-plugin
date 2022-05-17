module QueensD where
-- Compute the number of solutions to queens placements.
-- This implementation uses prelude operations and list comprehensions,
-- thus, higher-order operations like `map`.

import Plugin.CurryPlugin.Monad

{-# ANN module Nondeterministic #-}

queens :: Int -> Int
queens nq = length (gen nq nq)

gen :: Int -> Int -> [[Int]]
gen nq n = if n==0
          then [[]]
          else [ (q:b) | b <- gen nq (n-1), q <- [1..nq], safe q 1 b]

safe :: Int -> Int -> [Int] -> Bool
safe _ _ [] = True
safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

queens_10, queens_11 :: Int
queens_10 = queens 10
queens_11 = queens 11

main :: Int
main = queens_11

mainP :: Curry Int
mainP = liftE $ return queens_11
