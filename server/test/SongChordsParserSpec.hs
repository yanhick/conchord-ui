module SongChordsParserSpec where

import Test.Hspec
import SongChordsParser
import Data.Text

spec :: Spec
spec = do
  describe "SongChordsParser" $ do
      it "parses a song chord" $ do
        parseSongChords (pack "A 132456") `shouldSatisfy`
          (\x -> case x of
            Right _ -> True
            _ -> False)

      it "parses multiple song chords" $ do
        parseSongChords (pack "A 132456 \n B 123456") `shouldSatisfy`
          (\x -> case x of
            Right [_, _] -> True
            _ -> False)

      it "returns an error on wrong number of arguments" $ do
        parseSongChords (pack "too much args for chords") `shouldSatisfy`
          (\x -> case x of
            Left _ -> True
            _ -> False)

main :: IO ()
main = hspec spec
