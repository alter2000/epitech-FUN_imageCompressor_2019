module Logic
  where

import System.Random
import Data.Function ( on )
import Data.List ( genericLength )

import Control.Exception hiding (assert)
import Control.Monad ( forM_ )
import Control.Monad.ST

import Lib.Types
import Lib.Parser
import Lib.KMeansHelper
import Lib.KMeans

type Result = (Color, [Pixel])

calc :: Opts -> StdGen -> [Result]
calc (Opts k d ps) gen = until clustered (loopKMeans k) base
  where
    clustered = clusteredFor d
    base :: [Result]
    base = let ls = kmeansStep k ps
            in (meanColor <$> ls) `zip` ls

clusteredFor :: Double -> [Result] -> Bool
clusteredFor d ss = d > mean (means <$> (snd <$> ss))

means :: [Pixel] -> Double
means = const (-1)

loopKMeans :: Int -> [Result] -> [Result]
loopKMeans d = id

kmeansStep :: Int -> [Pixel] -> Clustering Pixel
kmeansStep = kmeansGen 3 toPoint

meanColor :: [Pixel] -> Color
meanColor cs = (mRed, mGreen, mBlue)
  where
    chew :: (Integral c, Real b) => (Pixel -> b) -> c
    chew f = round . mean $ map f cs
    mRed   = chew n1
    mGreen = chew n2
    mBlue  = chew n3

    f = fromIntegral
    n1 (Pixel _ (r, _, _)) = f r
    n2 (Pixel _ (_, r, _)) = f r
    n3 (Pixel _ (_, _, r)) = f r

mean :: (Fractional a, Real b) => [b] -> a
mean xs = realToFrac (sum xs) / genericLength xs
