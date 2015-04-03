module SongMetaParserSpec where

import Test.Hspec
import SongMetaParser

spec :: Spec
spec = do
  describe "SongMetaParser" $ do
    context "first test" $ do
      it "test" $ do
       True `shouldBe` True

main :: IO ()
main = hspec spec
