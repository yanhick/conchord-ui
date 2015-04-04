{-# LANGUAGE OverloadedStrings #-}
module SongSectionParser where

import Data.Maybe
import qualified Data.Text as T

type SongSectionName = T.Text
data SongSection = SongSection [Either SongSectionParserError SongSectionLine] deriving Show

type ChordPosition = Int
type ChordName = T.Text
type Lyrics = T.Text
type Annotation = T.Text

data SongSectionLine
  = LyricsLine Lyrics
  | ChordsLine [(ChordName, ChordPosition)]
  | SectionName SongSectionName
  | AnnotationLine Annotation
  | EmptyLine
  deriving Show

type SongSectionParserError = String

parseSongSectionLine :: T.Text -> Either SongSectionParserError SongSectionLine
parseSongSectionLine l
  | l == "" =  Right EmptyLine
  | T.take 2 l == "  " = parseLyricsOrChordsSectionLine (T.drop 2 l)
  | T.take 2 l == "//" = Right (AnnotationLine (T.drop 2 l))
  | otherwise = Right (SectionName l)

parseLyricsOrChordsSectionLine :: T.Text-> Either SongSectionParserError SongSectionLine
parseLyricsOrChordsSectionLine l
  | isJust (T.find (=='|') l) = parseChordsLine l
  | otherwise = Right (LyricsLine l)

parseChordsLine :: T.Text -> Either SongSectionParserError SongSectionLine
parseChordsLine l = Right (ChordsLine $ zip (splitChords l) (getChordIndices l 0))
  where splitChords cs = fmap T.strip (tail (T.splitOn "|" cs))
        getChordIndices cs i =
          case T.findIndex (=='|') cs of
            Just idx -> (idx + i) : getChordIndices (T.drop (idx + 1) cs) (i + idx + 1)
            Nothing -> []

parseSongSection :: T.Text -> SongSection
parseSongSection t = SongSection (fmap parseSongSectionLine (T.lines t))
