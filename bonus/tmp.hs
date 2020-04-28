module Lib.KMeans (kmeans, kmeansGen)
    where

import Data.List (transpose, sort, groupBy, minimumBy)
import Data.Function (on)
import Data.Ord (comparing)

data WrapType a = WrapType {getVect :: [Double], getVal :: a}
instance Eq (WrapType a) where
   (==) = (==) `on` getVect
instance Ord (WrapType a) where
    compare = comparing getVect

dist a b = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) a b

centroid points = map (flip (/) l . sum) $ transpose (map getVect points)
    where l = fromIntegral $ length points

closest points point = minimumBy (comparing $ dist point) points

recluster' centroids points = map (map snd) $ groupBy ((==) `on` fst) reclustered
    where reclustered = sort [(closest centroids (getVect a), a) | a <- points]

recluster clusters = recluster' centroids $ concat clusters
    where centroids = map centroid clusters

part :: (Eq a) => Int -> [a] -> [[a]]
part x ys
     | null zs' = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

-- | Recluster points
kmeans'' cs
    | cs == cs' = cs
    | otherwise = kmeans'' cs'
    where cs' = recluster cs

kmeans' k points = kmeans'' $ part l points
    where l = (length points + k - 1) `div` k

-- | Cluster points in a Euclidian space, represented as lists of Doubles, into at most k clusters.
-- The initial clusters are chosen arbitrarily.
kmeans :: Int -> [[Double]] -> [[[Double]]]
kmeans = kmeansGen id

-- | A generalized kmeans function. This function operates not on points, but an arbitrary type which may be projected into a Euclidian space. Since the projection may be chosen freely, this allows for weighting dimensions to different degrees, etc.
kmeansGen :: (a -> [Double]) -> Int -> [a] -> [[a]]
kmeansGen f k = map (getVal <$>) . kmeans' k . map wrap
  where
    wrap x = WrapType (f x) x
