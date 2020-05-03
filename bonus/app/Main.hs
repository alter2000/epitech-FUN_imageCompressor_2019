{-# LANGUAGE TypeApplications #-}

module Main where

import System.Exit       ( exitSuccess )
import Control.Exception ( catch, SomeException )
import System.Random     ( newStdGen )

import Prog
import Logic

main :: IO ()
main = do
  opts <- catch parseArgs (except @SomeException)
  case opts of
    Just os -> do
      g <- newStdGen
      catch (printOut $ calc os g)
            (except @SomeException)
    Nothing -> exitSuccess
