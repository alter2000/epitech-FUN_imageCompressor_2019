{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-structural-termination" @-}

module Lib.Parser
  where

import Lib.KMeansHelper
import Lib.Types

{-@ toRepr :: v:Color -> ColorRepr  @-}
toRepr :: Color -> ColorRepr
toRepr (r, g, b) = map fromIntegral [r, g, b]

{-@ fromRepr :: v:ColorRepr -> Color @-}
fromRepr :: ColorRepr -> Color
fromRepr [r, g, b] = (round r, round g, round b)
fromRepr _ = error "Lib.Parser.fromRepr: invalid color representation, this error should have never shown"

{-@ toPoint :: Pixel -> Point 3 @-}
toPoint :: Pixel -> Point 3
toPoint (Pixel _ (r, g, b)) =
  fromIntegral <$> [r, g, b]

getPixels :: String -> [Pixel]
getPixels = (read @Pixel <$>) . lines
