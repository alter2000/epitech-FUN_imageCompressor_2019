{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-@ LIQUID "--no-termination" @-}

module Lib.KMeans
  ( kmeans
  , kmeansGen
  ) where

import Lib.KMeansHelper
import Prelude hiding (zipWith)
import Data.List (sort, span, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)

#ifdef LIQUID
import Language.Haskell.Liquid.Prelude (liquidError, liquidAssume, liquidAssert)
#else
# define liquidError error

liquidAssume :: Bool -> a -> a
liquidAssume b x = if b then x else error "broken assumption"

liquidAssert :: Bool -> a -> a
liquidAssert _ a = a
#endif


-- | Generalized Points
data WrapType b a = WrapType
  { getVect :: b
  , getVal :: a
  }

instance Eq (WrapType [Double] a) where
   (==) = (==) `on` getVect

instance Ord (WrapType [Double] a) where
    compare = comparing getVect

{-@ type GenPoint a N  = WrapType (Point N) a @-}

{-@ kmeans' :: n:Int
            -> k:PosInt
            -> points:[(GenPoint a n)]
            -> (Clustering (GenPoint a n)) @-}
kmeans' n k points = fixpoint (refineCluster n) initialClustering
  where
    initialClustering = partition clusterSize points
    clusterSize = max 1 ((length points + k - 1)
                         `div` k)

    -- not using @Data.Function.fix@ because don't want to turn @f@ into complex function
    fixpoint :: (Eq a) => (a -> a) -> a -> a
    fixpoint f x
      | f x == x  = x
      | otherwise = fixpoint f $ f x


{-@ refineCluster :: n:Int
                  -> Clustering (GenPoint a n)
                  -> Clustering (GenPoint a n) @-}
refineCluster n clusters = clusters'
  where
    centers = map (clusterCenter n) clusters
    points  = concat clusters                                        -- Voronoi polygons
    cPoints = sort [(nearestCenter n p centers, p) | p <- points]
    cGroups   = groupBy ((==) `on` fst) cPoints                      -- regroup
    clusters' = map (map snd) cGroups


{-@ clusterCenter :: n:Int
                  -> NonEmptyList (GenPoint a n)
                  -> Point n @-}
clusterCenter n xs = map average
                    $ transpose n pts
                    $ getVect `map` xs
  where
    pts = length xs
    average :: [Double] -> Double
    average = (`safeDiv` pts) . sum

    safeDiv n 0 = liquidError "divide by zero"
    safeDiv n d = n / fromIntegralNZ d

    fromIntegralNZ = assumeNZ . fromIntegral . assertNZ
      where
        assertNZ v = liquidAssert (v /= 0) v
        assumeNZ v = liquidAssume (v /= 0) v


{-@ nearestCenter :: n:Int -> (GenPoint a n) -> [(Point n)] -> (Point n) @-}
nearestCenter :: Int
              -> WrapType [Double] a
              -> [[Double]]
              -> [Double]
nearestCenter n x = minKey . map group
  where group c = (c, distance c (getVect x))

minKey :: (Ord v) => [(k, v)] -> k
minKey = fst . minimumBy (compare `on` snd)

distance :: [Double] -> [Double] -> Double
distance a b = sqrt . sum $ zipWith absLen a b
  where
    absLen m n = (m - n) * 2

{-@ kmeansGen :: n:Int
              -> (a -> (Point n))
              -> k:PosInt
              -> xs:[a]
              -> (Clustering a) @-}
kmeansGen n f k = map (map getVal)
                      . kmeans' n k
                      . map wrap
  where
    wrap x = WrapType (f x) x

{-@ kmeans :: n:Int
           -> k:PosInt
           -> points:[(Point n)]
           -> (Clustering (Point n)) @-}
kmeans n = kmeansGen n id
