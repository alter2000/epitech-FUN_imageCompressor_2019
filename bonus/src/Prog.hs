{-# LANGUAGE BlockArguments #-}
module Prog
  where

import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPrint, stderr )
import System.Environment ( getArgs )
import Control.Exception  ( throw, AssertionFailed(..) )

import Lib.Types
import Lib.Parser ( getPixels )
import Logic

parseArgs :: IO (Maybe Opts)
parseArgs = do
  as <- getArgs
  case as of
    [] -> helpText >> return Nothing
    [k, cl, fpath] -> do
      let k'  = read k
          cl' = read cl
      ps <- readFile fpath
      let ps' = getPixels ps
      return . Just $ Opts
        (assert (k' > 0 && k' <= length ps) k'
                "n must be natural and smaller than the dataset")
        (assert (cl' > 0) cl' "e must be positive")
        ps'
    _ -> error "Invalid arguments.\n"

printOut :: [Result] -> IO ()
printOut = mapM_ \(c, ps) -> do
  putStrLn "---"
  print c
  putStrLn "-"
  mapM_ print ps

except :: Show a => a -> IO b
except e = hPrint stderr e >> exitWith (ExitFailure 84)

assert :: Bool -> a -> String -> a
assert b a msg = if b then a else throw (AssertionFailed msg)

helpText :: IO ()
helpText = putStr $ unlines
    [ "USAGE ./imageCompressor n e IN"
    , ""
    , "\tn\tnumber of colors in the final image"
    , "\te\tconvergence limit"
    , "\tIN\tpath to the file containing the colors of the pixels"
    ]
