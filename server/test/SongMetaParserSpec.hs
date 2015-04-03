module SongMetaParserSpec where

import Test.Hspec
import SongMetaParser

spec :: Spec
spec = do
  describe "SongMetaParser" $ do
      it "parses a song meta" $ do
        parseSongMeta "title song" `shouldBe` [Right (SongMeta (Standard Title, "song"))]

      it "parses a list of song meta" $ do
        parseSongMeta "title song\nartist name" `shouldBe`
          [Right (SongMeta (Standard Title, "song")),
          Right (SongMeta (Standard Artist, "name"))]

      it "returns an error on wrong format" $ do
        parseSongMeta "too much words" `shouldBe` [Left "wrong number of args"]

main :: IO ()
main = hspec spec
