module Main where

import Control.Exception (catch)
import Prog

main :: IO ()
main = do
  input <- catch parseArgs except
  printOut $ calc input
