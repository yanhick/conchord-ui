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
            SongSection [
              Right (SectionName _),
              Right (LyricsLine _),
              Right (ChordsLine _),
              Right (AnnotationLine _) ] -> True
            _ -> False)

main :: IO ()
main = hspec spec
