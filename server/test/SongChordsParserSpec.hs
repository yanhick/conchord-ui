module SongChordsParserSpec where

import Test.Hspec
import SongChordsParser

spec :: Spec
spec = do
  describe "SongChordsParser" $ do
      it "parses a song chord" $ do
        parseSongChords "A 123456" `shouldSatisfy`
          (\x -> case x of
            [Right _] -> True
            _ -> False)

      it "parses multiple song chords" $ do
        parseSongChords "A 132456 \n B 123456" `shouldSatisfy`
          (\x -> case x of
            [Right _, Right _] -> True
            _ -> False)

      it "returns an error on wrong number of arguments" $ do
        parseSongChords "too much args for chords" `shouldSatisfy`
          (\x -> case x of
            [Left _] -> True
            _ -> False)

main :: IO ()
main = hspec spec
