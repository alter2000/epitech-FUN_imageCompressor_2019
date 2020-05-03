module Logic
  where

import Data.List ( genericLength )
import Data.Function ( on )

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
             -> Clustering Pixel
             -> Clustering Pixel
             -> Bool
clusteredFor e old new = e > maximum distance'
  where
    oldCentroids, newCentroids :: [Color]
    oldCentroids = let ret = map meanColor old
                    -- in trace ("old: " ++ show old ++ "\nsent: " ++ show ret) ret
                    in ret
    newCentroids = let ret = map meanColor new
                    -- in trace ("new: " ++ show new ++ "\nsent: " ++ show ret) ret
                    in ret

    distance' :: [Double]
    distance' = zipWith rgbLen oldCentroids newCentroids

    rgbLen :: Color -> Color -> Double
    rgbLen (r, g, b) (r', g', b') = let ret = (distance `on` map fromIntegral) [r, g, b] [r', g', b']
                                     -- in trace (show x ++ " : " ++ show y ++ "\ndist: " ++ show r) ret
                                     in ret

meanColor :: [Pixel] -> Color
meanColor cs = (mRed cs, mGreen cs, mBlue cs)

chew :: Integral c => (a -> Double) -> [a] -> c
chew g = round . mean . map g


mRed, mGreen, mBlue :: Integral c => [Pixel] -> c
mRed   a = chew n1 a
mGreen a = chew n2 a
mBlue  a = chew n3 a

n1, n2, n3 :: Num b => Pixel -> b
n1 (Pixel _ (r, _, _)) = fromIntegral r
n2 (Pixel _ (_, r, _)) = fromIntegral r
n3 (Pixel _ (_, _, r)) = fromIntegral r

mean :: [Double] -> Double
mean xs = realToFrac (sum xs) / xlen xs
  where xlen [] = error "Logic.mean: zero length list"
        xlen a  = genericLength a
