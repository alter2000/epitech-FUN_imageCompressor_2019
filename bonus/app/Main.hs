{-# LANGUAGE TypeApplications #-}

module Main where

import System.Exit       ( exitWith, ExitCode (..) )
import Control.Exception ( catch, SomeException )
import Prog

main :: IO ()
main = do
  opts <- catch parseArgs (except @SomeException)
  case opts of
    Just os -> catch (printOut $ calc os)
                     (except @SomeException)
    Nothing -> exitWith ExitSuccess
