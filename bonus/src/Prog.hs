{-# LANGUAGE TypeApplications #-}
module Prog
  where

import System.IO ( hPrint, stderr )
import System.Exit
import System.Environment ( getArgs )
import Data.Function ((&))
import Control.Exception
import Text.ParserCombinators.ReadP
import Lib.KMeans
import Lib.Parser

type Result = [(Color, [Pixel])]
data Opts = Opts { k  :: Int
                 , cl :: Double
                 , fp :: [Pixel]
                 }
  deriving (Show, Read, Eq)

except :: Show a => a -> IO b
except e = hPrint stderr e >> exitWith (ExitFailure 84)

printOut :: Result -> IO ()
printOut xs = print xs

helpText :: IO ()
helpText = putStr $ unlines
    [ "USAGE ./imageCompressor n e IN"
    , ""
    , "\tn\tnumber of colors in the final image"
    , "\te\tconvergence limit"
    , "\tIN\tpath to the file containing the colors of the pixels"
    ]

parseArgs :: IO (Maybe Opts)
parseArgs = do
  as <- getArgs
  case as of
    [k, cl, fpath] -> do
      file <- readFile fpath
      return . Just $ Opts (read k) (read cl) $ read @Pixel <$> (lines file)
    [] -> helpText >> return Nothing
    _ -> error "Invalid arguments.\n"

calc :: (Int, Double, [Pixel]) -> Result
calc = undefined
