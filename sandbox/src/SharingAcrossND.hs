{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
module SharingAcrossND where

import SortPrimes

prime800 :: Int
prime800 = primes !! 800

testSharing :: Int
testSharing =
  let p = prime800
  in p ? p

testNoSharing :: Int
testNoSharing =
  prime800 ? prime800
