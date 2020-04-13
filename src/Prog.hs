module Prog
  where

import System.IO ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode (..) )
import Data.Function ((&))
import Control.Exception
import Text.ParserCombinators.ReadP
import Lib.KMeans

data Result = Result
data Vals = Vals

except :: SomeException -> IO a
except e = hPutStrLn stderr (show e) >> exitWith (ExitFailure 84)

printOut :: Result -> IO ()
printOut xs = separate xs
            & group
            & splitMeans
            & printAll

parseArgs :: IO Vals
parseArgs = undefined

separate = undefined
group = undefined
splitMeans = undefined
printAll = undefined
calc = undefined
