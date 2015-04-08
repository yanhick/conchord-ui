module SongMetaParserSpec where

import Test.Hspec
import SongMetaParser
import Data.Text

spec :: Spec
spec = do
  describe "SongMetaParser" $ do
      it "parses a song meta" $ do
        parseSongMeta (pack "title song") `shouldBe` Right [(SongMeta (Standard Title, (pack "song")))]

      it "parses a list of song meta" $ do
        parseSongMeta (pack "title song\nartist name") `shouldBe`
          Right [(SongMeta (Standard Title, (pack "song"))),
                 (SongMeta (Standard Artist, (pack "name")))]

      it "returns an error on wrong format" $ do
        parseSongMeta (pack "too much words") `shouldBe` Left "wrong number of args"

main :: IO ()
main = hspec spec
