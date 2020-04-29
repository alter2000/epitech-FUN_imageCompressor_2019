{-# LANGUAGE TypeApplications #-}

module Main where

import System.Exit       ( exitSuccess )
import Control.Exception ( catch, SomeException )
import System.Random     ( randoms, getStdGen )

import Prog
import Logic

main :: IO ()
main = do
  opts <- catch parseArgs (except @SomeException)
  case opts of
    Just os -> do
      gen <- getStdGen
      catch (printOut $ calc os gen)
            (except @SomeException)
    Nothing -> exitSuccess
