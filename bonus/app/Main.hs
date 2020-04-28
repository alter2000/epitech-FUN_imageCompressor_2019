{-# LANGUAGE TypeApplications #-}

module Main where

import System.Exit       ( exitWith, ExitCode (..) )
import Control.Exception ( catch, SomeException )
import Prog

main :: IO ()
main = do
  opts <- catch parseArgs (except @SomeException)
  case opts of
    Nothing ->
      exitWith ExitSuccess
    Just (Opts k cl f) ->
      catch (printOut . calc $ (k, cl, f))
            (except @SomeException)
