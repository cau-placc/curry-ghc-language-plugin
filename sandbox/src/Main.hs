{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module Main where

import System.Environment (getArgs)

import qualified AddNum
import qualified NRev
import qualified PermSort
import qualified PermSortPeano
import qualified PrimesHO
import qualified Queens
import qualified ReverseHO
import qualified Select
import qualified SortPrimes
import qualified TakInt
import qualified TakPeano

import Example2
import Plugin.CurryPlugin.Encapsulation

mainOLD :: IO ()
mainOLD = putStrLn $ "There are " ++ show (length res) ++
                  " permutations of a list of length " ++ show n
  where
    n = 3
    res = $(evalGeneric DFS 'permutations) [(1::Int)..n]

main :: IO ()
main = do
  args <- getArgs
  res <- case args of
         [x] -> case x of
           "AddNum"        -> return $ show $ eval DFS $ AddNum.main
           "NRev"          -> return $ show $ eval DFS $ NRev.main
           "PermSort"      -> return $ show $ eval DFS $ PermSort.main
           "PermSortPeano" -> return $ show $ eval DFS $ PermSortPeano.main
           "PrimesHO"      -> return $ show $ eval DFS $ PrimesHO.main
           "Queens"        -> return $ show $ eval DFS $ Queens.main
           "ReverseHO"     -> return $ show $ eval DFS $ ReverseHO.main
           "Select"        -> return $ show $ eval DFS $ Select.main
           "SortPrimes"    -> return $ show $ eval DFS $ SortPrimes.main
           "TakInt"        -> return $ show $ eval DFS $ TakInt.main
           "TakPeano"      -> return $ show $ eval DFS $ TakPeano.main
           _               -> fail $ "unexpected argument: " ++ x
         _ -> fail $ "expected 1 argument, but got: " ++ show (length args)
  putStrLn res

deriving instance Show (TakPeano.Nat)
deriving instance Show (ReverseHO.MyBool)
deriving instance Show a => Show (PermSortPeano.List a)
deriving instance Show (PermSortPeano.Nat)
