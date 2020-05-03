{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-@ LIQUID "--no-termination" @-}

module Lib.KMeans
  ( kmeansGen
  , partition
  , WrapType(..)
  ) where

import Prelude hiding ( zipWith )
import Data.Ord       ( comparing )
import Data.List      ( sort, minimumBy )
import Data.Function  ( on )

import Lib.KMeansHelper

#ifdef LIQUID
import Language.Haskell.Liquid.Prelude ( liquidError, liquidAssume, liquidAssert )
#else
import Control.Exception (assert)
# define liquidError error

liquidAssume :: Bool -> a -> a
liquidAssume b x = if b then x else error "Lib.KMeans.liquidAssume: broken assumption"

liquidAssert :: Bool -> a -> a
liquidAssert = assert
#endif

data WrapType b a = WrapType
  { cmp :: b
  , val :: a
  }

instance Eq (WrapType [Double] a) where
   (==) = (==) `on` cmp

instance Ord (WrapType [Double] a) where
    compare = comparing cmp

instance Show a => Show (WrapType [Double] a) where
  show (WrapType _ a) = show a

{-@ type GenPoint a N  = WrapType (Point N) a @-}


{-@ kmeans' :: n:Int
            -> k:PosInt
            -> points:[(GenPoint a n)]
            -> (Clustering (GenPoint a n))
  @-}
kmeans' :: Int
        -> Int
        -> (Clustering a -> Clustering a -> Bool)
        -> [WrapType [Double] a]
        -> Clustering (WrapType [Double] a)
kmeans' n k cmpF pts = nonfix (cmpF `on` unwrap) (refineCluster n) initC
  where
    unwrap = map (map val)
    initC = partition cSize pts
    cSize = max 1 $ div (length pts + k - 1) k

    -- TODO: help
    nonfix :: ([a] -> [a] -> Bool) -> ([a] -> [a]) -> [a] -> [a]
    nonfix cc f !x | cc x new  = x
                   | otherwise = nonfix cc f new
      where !new = f x


{-@ refineCluster :: n:Int
                  -> Clustering (GenPoint a n)
                  -> Clustering (GenPoint a n)
  @-}
-- TODO: maybe supply tools to use @fix@?
refineCluster :: Int
              -> Clustering (WrapType [Double] a)
              -> Clustering (WrapType [Double] a)
refineCluster n old = new
  where
    centers = map (clusterCenter n) old
    points  = concat old
    -- ^ Voronoi polygons, drop meta
    centeredPoints = sort [(nearestCenter n p centers, p) | p <- points]
    centeredGroups = groupBy ((==) `on` fst) centeredPoints
    -- ^ _CAN_ be empty on groupBy
    new = map (map snd) centeredGroups

{-@ partition :: size:PosInt -> [a] -> (Clustering a) @-}
partition :: Int -> [a] -> Clustering a
partition _ [] = []
partition size ys = case splitAt size ys of
  (ys', [])  -> [ys']
  (beg, end) -> beg : partition size end


{-@ clusterCenter :: n:Int -> NonEmptyList (GenPoint a n) -> Point n @-}
clusterCenter :: Int -> [WrapType [Double] a] -> [Double]
clusterCenter n xs = map average xs'
  where
    numPoints = length xs
    xs' = transpose n numPoints (map cmp xs)

    average :: [Double] -> Double
    average = (`safeDiv` numPoints) . sum

safeDiv :: Double -> Int -> Double
safeDiv _ 0 = liquidError "divide by zero"
safeDiv n d = n / fromIntegralNZ d

fromIntegralNZ :: Int -> Double
fromIntegralNZ = assumeNZ . fromIntegral . assertNZ
  where
    assertNZ v = liquidAssert (v /= 0) v
    assumeNZ v = liquidAssume (v /= 0) v


{-@ nearestCenter :: n:Int -> (GenPoint a n) -> [(Point n)] -> (Point n) @-}
nearestCenter     :: Int -> WrapType [Double] a -> Clustering Double -> [Double]
nearestCenter _ x = minKey . map (\c -> (c, distance c (cmp x)))


minKey :: (Ord v) => [(k, v)] -> k
minKey = fst . minimumBy (compare `on` snd)


{-@ kmeansGen :: n:Int
              -> (a -> (Point n))
              -> k:PosInt
              -> xs:[a]
              -> (Clustering a)
  @-}
kmeansGen :: Int
          -> (a -> [Double])
          -> (Clustering a -> Clustering a -> Bool)
          -> Int
          -> [a]
          -> Clustering a
kmeansGen n implF cmpF k = unwrap
                      . kmeans' n k cmpF
                      . map wrap
  where
    wrap x = WrapType (implF x) x
    unwrap = map (map val)
