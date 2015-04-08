module SongSectionParserSpec where

import Test.Hspec
import Data.Text
import SongSectionParser

spec :: Spec
spec = do
  describe "SongSectionParser" $ do
      it "parses a song section" $ do
        parseSongSection (pack "NAME\n  lyrics\n  |A\n//") `shouldSatisfy`
          (\x -> case x of
            Right (SongSection [
              SectionName _,
              LyricsLine _,
              ChordsLine _,
              AnnotationLine _ ]) -> True
            _ -> False)

main :: IO ()
main = hspec spec
