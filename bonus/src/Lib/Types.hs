{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-structural-termination" @-}

module Lib.Types
  where

import Data.Word ( Word8 )
import Lib.KMeansHelper ( Point )

data Opts = Opts { k  :: Int
                 , cl :: Double
                 , ps :: [Pixel]
                 }
  deriving (Show, Read, Eq)

{-@ type X = PosInt @-}
type X = Int
{-@ type Y = PosInt @-}
type Y = Int
type Coord = (X, Y)

{-@ type Byte = { v: Nat | v < 256}  @-}
newtype Byte = Byte Word8
  deriving (Enum, Eq, Ord, Bounded, Num, Real)
type R = Byte
type G = Byte
type B = Byte
type Color = (Byte, Byte, Byte)

data Pixel = Pixel Coord Color
  deriving (Eq, Ord, Bounded)

{-@ type ColorRepr = Point 3 @-}
type ColorRepr = Point 3


instance Read Byte where
  readsPrec _ (reads @Int -> [(b, s)])
    | b < 0 || b > 255 = []
    | otherwise = [(fromIntegral b, s)]
  readsPrec _ _ = []

instance Integral Byte where
  toInteger (Byte b) = fromIntegral b
  quotRem (Byte b)   = quotRem . fromIntegral $ b

instance Show Byte where
  show = show . fromIntegral

instance Read Pixel where
  readsPrec _ s = do
    let [pos, color] = splitWhen (==' ') s
    (p, "") <- reads @Coord pos
    (c, "") <- reads @Color color
    pure (Pixel p c, "")

instance Show Pixel where
  show (Pixel coord color) = show coord ++ " " ++ show color

{-@ splitWhen :: (Char -> Bool) -> String -> [String] @-}
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : splitWhen p s''
        where (w, s'') = break p s'
