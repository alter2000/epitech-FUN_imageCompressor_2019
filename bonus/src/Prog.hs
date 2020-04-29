{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Prog
  where

import System.IO ( hPrint, stderr )
import System.Exit
import System.Environment ( getArgs )
import Data.Function
import Data.List
import Control.Monad
import Control.Exception hiding (assert)
import Text.ParserCombinators.ReadP
import Lib.KMeansHelper
import Lib.KMeans
import Lib.Parser

instance Exception String

type Result = [Clustering Pixel]
data Opts = Opts { k  :: Int
                 , cl :: Double
                 , ps :: [Pixel]
                 }
  deriving (Show, Read, Eq)

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
      return . Just $ Opts (assert (k' > 0 && k' <= length ps) k'
                                   "n must be natural and smaller than the dataset")
                           (assert (cl' > 0) cl' "e must be positive")
                           ps'
    _ -> error "Invalid arguments.\n"

printOut :: Result -> IO ()
printOut = mapM_ showCluster
  where
    showCluster :: Clustering Pixel -> IO ()
    showCluster cs = forM_ cs $ \c -> do
      putStrLn "---"
      print $ findMean c
      putStrLn "-"
      mapM_ print c

findMean :: [Pixel] -> Color
findMean cs = (mRed, mGreen, mBlue)
  where
    mean :: (Fractional a, Real b) => [b] -> a
    mean xs = realToFrac (sum xs) / genericLength xs

    chew :: (Integral c, Real b) => (Pixel -> b) -> c
    chew f = round . mean $ map f cs
    mRed   = chew n1
    mGreen = chew n2
    mBlue  = chew n3

    f = fromIntegral
    n1 (Pixel _ (r, _, _)) = f r
    n2 (Pixel _ (_, r, _)) = f r
    n3 (Pixel _ (_, _, r)) = f r

calc :: Opts -> Result
calc (Opts k d ps) = [kmeansGen 3 toPoint k ps]

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
