{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
module PermSort where

insert :: a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = x:y:ys ? y : (insert x ys)

perm :: [a] -> [a]
perm [] = []
perm (x:xs) = insert x (perm xs)

sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) | x <= y = x : sorted (y:ys)
sorted _        = failed

psort :: [Int] -> [Int]
psort xs = sorted (perm xs)

sortDescList :: Int -> [Int]
sortDescList n = psort (2:[n,n-1 .. 3]++[1])

--main = psort [2,13,12,11,10,9,8,7,6,5,4,3,1]
main :: [Int]
main = sortDescList 13
