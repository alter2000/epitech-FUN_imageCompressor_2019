{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import System.IO.Silently
import System.Environment
import Prog
import Lib.Parser

instance Arbitrary Byte  where arbitrary = Byte  <$> arbitrary
instance Arbitrary Pixel where arbitrary = Pixel <$> arbitrary <*> arbitrary
instance Arbitrary Opts  where arbitrary = Opts  <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = hspec do

  describe "Lib.Parser.Byte" do
    success 1000 $ it "follows (Read, Show) laws" $ property $
      \x -> read (show x) == (x :: Byte)

  describe "Lib.Parser.Pixel" do
    success 1000 $ it "follows (Read, Show) laws" $ property $
      \x -> read (show x) == (x :: Pixel)

  describe "Lib.Parser.toRepr" do
    success 1000 $ it "is the inverse of fromRepr" $ property $
      \x -> fromRepr (toRepr x) == x

  describe "Lib.Parser.toPoint" do
    success 1000 $ it "returns valid `Point 3`" $ property $
      \x@(Pixel _ (r, g, b)) -> toPoint x == (fromIntegral <$> [r, g, b])

  describe "Prog.parseArgs" do
    it "drops invalid arguments" do
      withArgs [] (capture parseArgs) `shouldReturn` (helpMsg, Nothing)
    it "parses valid arguments" do
      withArgs ["2", "0.8", "test/testfile"] parseArgs
        `shouldReturn` Just
        (Opts { k = 2 , cl = 0.8 , fp = inFileContents })
    it "throws IOError on invalid file read" do
      withArgs ["2", "0.8", "yeeeeeeeet"] parseArgs
        `shouldThrow` anyIOException
    it "calls @error@ on invalid arguments" do
      withArgs ["2", "0.8"] parseArgs
        `shouldThrow` anyErrorCall

  describe "Prog.calc" do
    it "returns base result" do
      calc inArgs `shouldBe` out

helpMsg :: String
helpMsg = unlines [ "USAGE ./imageCompressor n e IN"
                  , ""
                  , "\tn\tnumber of colors in the final image"
                  , "\te\tconvergence limit"
                  , "\tIN\tpath to the file containing the colors of the pixels"
                  ]

inArgs :: (Int, Double, [Pixel])
inArgs = (2, 0.8, inFileContents)

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

out =
  [ ((77, 63, 204), [ Pixel (0, 1) (33, 18, 109)
                    , Pixel (0, 2) (33, 21, 109) ])
  , ((35, 36, 45),  [ Pixel (0, 0) (33, 18, 109)
                    , Pixel (1, 0) (33, 18, 109)
                    , Pixel (1, 1) (35, 18, 109)
                    , Pixel (1, 2) (35, 21, 109)
                    ])
  ]

success = modifyMaxSuccess . const
