module Main where

import Example
import Plugin.ParsePlugin.Monad
import Prelude hiding (Maybe(..)) 
import qualified Prelude as P
import Plugin.ParsePlugin.Prelude

main :: IO ()
main = return ()

test :: String -> P.Maybe Integer
test s = case parse parseAnBn s of 
  [("", Just i)] -> P.Just i
  _              -> P.Nothing