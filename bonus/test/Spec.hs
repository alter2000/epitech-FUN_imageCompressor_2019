{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import System.IO.Silently
import System.Environment
import Control.Exception (evaluate)
import Prog
import Lib.Parser

main :: IO ()
main = hspec do

  describe "Prog.parseArgs" do
    it "drops invalid arguments" do
      withArgs [] (capture parseArgs) `shouldReturn` (helpMsg, Nothing)
    it "parses valid arguments" do
      withArgs ["2", "0.8", "test/testfile"] parseArgs
        `shouldReturn` Just
        (Opts { k = 2 , cl = 0.8 , fp = inFileContents })

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
