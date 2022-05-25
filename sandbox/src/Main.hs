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
import qualified QueensD
import qualified ReverseHO
import qualified Select
import qualified SortPrimes
import qualified TakInt
import qualified TakPeano

import qualified Example
import qualified Example2
import Plugin.CurryPlugin.Encapsulation

mainOLD :: IO ()
mainOLD = putStrLn $ "There are " ++ show (length res) ++
                  " permutations of a list of length " ++ show n
  where
    n = 3
    res = $(evalGeneric DFS 'Example2.permutations) [(1::Int)..n]

main :: IO ()
main = do
  args <- getArgs
  res <- case args of
         [x] -> case x of
           "AddNum"         -> return $ show $ eval DFS AddNum.main
           "NRev"           -> return $ show $ eval DFS NRev.main
           "PermSort"       -> return $ show $ eval DFS PermSort.main
           "PermSortPeano"  -> return $ show $ eval DFS PermSortPeano.main
           "PrimesHO"       -> return $ show $ eval DFS PrimesHO.main
           "Queens"         -> return $ show $ eval DFS Queens.main
           "ReverseHO"      -> return $ show $ eval DFS ReverseHO.main
           "Select"         -> return $ show $ eval DFS Select.main
           "SortPrimes"     -> return $ show $ eval DFS SortPrimes.main
           "TakInt"         -> return $ show $ eval DFS TakInt.main
           "TakPeano"       -> return $ show $ eval DFS TakPeano.main

           "YesSharingAcrossND" -> return $ show $ eval DFS PrimesHO.yesSharingAcrossND
           "NoSharingAcrossND"  -> return $ show $ eval DFS PrimesHO.noSharingAcrossND

           "AddNumD"        -> return $ show $ eval DFS AddNum.mainD
           "NRevD"          -> return $ show $ eval DFS NRev.mainD
           "PermSortD"      -> return $ show $ eval DFS PermSort.mainD
           "PermSortPeanoD" -> return $ show $ eval DFS PermSortPeano.mainD
           "PrimesHOD"      -> return $ show $ eval DFS PrimesHO.mainD
           "QueensD"        -> return $ show $ eval DFS Queens.mainD
           "ReverseHOD"     -> return $ show $ eval DFS ReverseHO.mainD
           "SelectD"        -> return $ show $ eval DFS Select.mainD
           "SortPrimesD"    -> return $ show $ eval DFS SortPrimes.mainD
           "TakIntD"        -> return $ show $ eval DFS TakInt.mainD
           "TakPeanoD"      -> return $ show $ eval DFS TakPeano.mainD

           "YesSharingAcrossNDD" -> return $ show $ eval DFS PrimesHO.yesSharingAcrossNDD
           "NoSharingAcrossNDD"  -> return $ show $ eval DFS PrimesHO.noSharingAcrossNDD

           "QueensHs"       -> return $ show [QueensD.main]
           "id"             -> return $ show $ eval1 DFS Example.id True
           "mainOld"        -> mainOLD >> return ""
           _                -> fail $ "unexpected argument: " ++ x
         _ -> fail $ "expected 1 argument, but got: " ++ show (length args)
  putStrLn res

deriving instance Show a => Show (PermSortPeano.List a)
