{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-@ LIQUID "--no-termination" @-}

module Lib.KMeansHelper
  ( List
  , Point
  , Clustering
  , groupBy
  , zipWith
  , transpose
  , distance
  ) where

import Prelude hiding ( zipWith )
import Data.List      ( span )
import GHC.TypeNats   ( Nat )
#ifdef LIQUID
import Language.Haskell.Liquid.Prelude ( liquidError )
#else
# define liquidError error
#endif

-- | typechecked list
{-@ type List a N = {v : [a] | (len v) = N} @-}
type List a (ph :: Nat) = [a]
-- | list of doubles for position keeping
{-@ type Point N = List Double N @-}
type Point ph = List Double ph
{-@ type NonEmptyList a = {v : [a] | (len v) > 0} @-}
type NonEmptyList a = [a]
{-@ type Clustering a = [NonEmptyList a] @-}
type Clustering a = [NonEmptyList a]
-- | Position
{-@ type PosInt = {v: Int | v > 0 } @-}

-- | TODO: always keeps a single element around
{-@ groupBy :: (a -> a -> Bool) -> [a] -> (Clustering a) @-}
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
  where (ys, zs) = span (eq x) xs

{-@ span :: (a -> Bool) -> NonEmptyList a -> (NonEmptyList a, [a]) @-}


{-@ zipWith :: (a -> b -> c) -> xs:[a] -> (List b (len xs)) -> (List c (len xs)) @-}
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ [] [] = []
zipWith _  _ [] = liquidError "Lib.KMeansHelper.zipWith: dead code"
zipWith _ []  _ = liquidError "Lib.KMeansHelper.zipWith: dead code"

{-@ type Matrix a Rows Cols = (List (List a Cols) Rows) @-}

{-@ transpose :: c:Int -> r:PosInt -> Matrix a r c -> Matrix a c r @-}
transpose :: Show a => Int -> Int -> [[a]] -> [[a]]
transpose 0 _ _ = []
transpose c r ((x:xs) : xss) = (x : map head xss) : transpose (c-1) r (xs : map tail xss)
transpose _ _ ([] : _) = liquidError "Lib.KMeansHelper.transpose: dead code"
transpose _ _ [] = liquidError "Lib.KMeansHelper.transpose: dead code"
-- transpose c r ((x:xs) : xss) = (x : [ xs' | (x':_) <- xss ])
--                               : transpose (c - 1) r (xs : [xs' | (_ : xs') <- xss])


distance :: [Double] -> [Double] -> Double
distance a b = sqrt . sum $ zipWith absLen a b
  where absLen x x' = (x - x') ^ 2
