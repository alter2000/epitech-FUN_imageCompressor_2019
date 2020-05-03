{-# LANGUAGE BlockArguments #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import System.IO.Silently

import System.Environment
import System.Random

import Logic
import Prog
import Lib.Parser
import Lib.Types

instance Arbitrary Byte  where arbitrary = Byte  <$> arbitrary
instance Arbitrary Pixel where arbitrary = Pixel <$> arbitrary <*> arbitrary
instance Arbitrary Opts  where arbitrary = Opts  <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = hspec do

  describe "Lib.Parser.Byte" .
    success 10000 . it "follows (Read, Show) laws" . property $
      \x -> read (show x) == (x :: Byte)

  describe "Lib.Parser.Pixel" .
    success 10000 . it "follows (Read, Show) laws" . property $
      \x -> read (show x) == (x :: Pixel)

  describe "Lib.Parser.toRepr" .
    success 10000 . it "is the inverse of fromRepr" . property $
      \x -> fromRepr (toRepr x) == x

  describe "Lib.Parser.toPoint" .
    success 10000 $ it "returns valid `Point 3`" . property $
      \x@(Pixel _ (r, g, b)) -> toPoint x == (fromIntegral <$> [r, g, b])

  describe "Prog.parseArgs" $ do
    it "drops invalid arguments" $
      withArgs [] (capture parseArgs) `shouldReturn` (helpMsg, Nothing)
    it "parses valid arguments" $
      withArgs ["2", "0.8", "test/testfile"] parseArgs
        `shouldReturn` Just
        (Opts { k = 2 , cl = 0.8 , ps = inFileContents })
    it "throws IOError on invalid file read" $
      withArgs ["2", "0.8", "yeeeeeeeet"] parseArgs
        `shouldThrow` anyIOException
    it "calls @error@ on invalid arguments" $
      withArgs ["2", "0.8"] parseArgs
        `shouldThrow` anyErrorCall

  describe "Logic.calc" .
    it "returns base result" $ do
      calc inArgs (mkStdGen 0) `shouldBe` out

helpMsg :: String
helpMsg = unlines [ "USAGE ./imageCompressor n e IN"
                  , ""
                  , "\tn\tnumber of colors in the final image"
                  , "\te\tconvergence limit"
                  , "\tIN\tpath to the file containing the colors of the pixels"
                  ]

inArgs = Opts 2 0.8 inFileContents

inFileContents :: [Pixel]
inFileContents =
  [ Pixel (0, 0) (33, 18, 109)
  , Pixel (0, 1) (33, 18, 109)
  , Pixel (0, 2) (33, 21, 109)
  , Pixel (0, 3) (33, 21, 112)
  , Pixel (0, 4) (33, 25, 112)
  , Pixel (0, 5) (33, 32, 112)
  , Pixel (1, 0) (33, 18, 109)
  , Pixel (1, 1) (35, 18, 109)
  , Pixel (1, 2) (35, 21, 109)
  , Pixel (1, 3) (38, 21, 112)
  ]

out :: [(Color, [Pixel])]
out = [ ((33, 18, 109)
        , [ Pixel (0, 0) (33, 18, 109)
          , Pixel (0, 1) (33, 18, 109)
          , Pixel (1, 0) (33, 18, 109)
          ])
      , ((34, 23, 111)
          , [ Pixel (0, 2) (33, 21, 109)
            , Pixel (0, 3) (33, 21, 112)
            , Pixel (0, 4) (33, 25, 112)
            , Pixel (0, 5) (33, 32, 112)
            , Pixel (1, 1) (35, 18, 109)
            , Pixel (1, 2) (35, 21, 109)
            , Pixel (1, 3) (38, 21, 112)
            ]
        ) ]

success = modifyMaxSuccess . const
