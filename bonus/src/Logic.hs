module Logic
  where

import Data.List ( genericLength )

import Lib.Types
import Lib.Parser
import Lib.KMeansHelper hiding ( zipWith )
import Lib.KMeans

import System.Random

type Result = (Color, [Pixel])

calc :: Opts -> StdGen -> [Result]
calc (Opts k' e ps') seed = base
  where
    base :: [Result]
    base = let ls = kmeansStep e k' $ shuffle seed ps'
            in (meanColor <$> ls) `zip` ls

shuffle :: StdGen -> [a] -> [a]
shuffle _   [] = []
shuffle gen xs = el : shuffle gen' new
  where
    (idx, gen') = randomR (0, length xs - 1) gen
    el  = xs !! idx
    new = take idx xs ++ drop (idx+1) xs

-- shuffle :: [Int] -> [a] -> [a]
-- shuffle (i:is) xs = (last beg) : shuffle is (init beg ++ end)
--   where
--     (beg, end) = splitAt (i `mod` length xs) xs

kmeansStep :: Double
           -> Int -> [Pixel] -> Clustering Pixel
kmeansStep e = kmeansGen 3 toPoint (clusteredFor e)


clusteredFor :: Double
             -> Clustering (WrapType [Double] b)
             -> Clustering (WrapType [Double] b)
             -> Bool
clusteredFor e old new = e > maximum (distance' oldCentroids newCentroids)
  where
    distance' :: [[Double]] -> [[Double]] -> [Double]
    distance' = zipWith distance
    oldCentroids = means old
    newCentroids = means new

means :: Clustering (WrapType [Double] b) -> [[Double]]
means = map (meanC . map cmp)


meanC :: [[Double]] -> [Double]
meanC cs = [mRed, mGreen, mBlue]
  where
    chew g = mean $ map g cs
    mRed   = chew n1
    mGreen = chew n2
    mBlue  = chew n3

    n1 [r, _, _] = r
    n2 [_, r, _] = r
    n3 [_, _, r] = r

meanColor :: [Pixel] -> Color
meanColor cs = (mRed, mGreen, mBlue)
  where
    chew g = round . mean $ map g cs
    mRed   = chew n1
    mGreen = chew n2
    mBlue  = chew n3

    f = fromIntegral
    n1 (Pixel _ (r, _, _)) = f r
    n2 (Pixel _ (_, r, _)) = f r
    n3 (Pixel _ (_, _, r)) = f r

mean :: [Double] -> Double
mean xs = realToFrac (sum xs) / xlen xs
  where xlen [] = error "Logic.mean: zero length list"
        xlen a  = genericLength a
