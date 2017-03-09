module Model where

import Prelude (pure, class Show, bind, ($), (<$>), class Eq, (==), (&&), (<<<), (<>), show)

import Control.Alt ((<|>))
import Data.Foreign.Class (class IsForeign)
import Data.String (fromCharArray, joinWith)
import Data.Array (fromFoldable)
import Data.List (List)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Functor (($>))
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions)

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Generic (gArbitrary)

import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser.String (string, eof, anyChar, anyDigit)
import Text.Parsing.StringParser.Combinators (manyTill, lookAhead)

import Parser (SongChord, exampleChord, parseChord)

--- Search Model

type SearchResults = Array DBSong

--- Songs list Model

type SongsList = Array DBSong

--- Song Model

newtype DBSong = DBSong {
    id :: String,
    song :: Song
}

newtype Song = Song {
    meta :: SongMeta,
    content :: SongContent
}

newtype SongMeta = SongMeta {
    title :: String,
    artist :: String,
    album :: String,
    year :: Year
}

newtype Year = Year Int

newtype SongContent = SongContent (Array SongSection)

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

data SongLyric = ChordAndLyric ChordPlacement String | OnlyChord SongChord | OnlyLyric String

data ChordPlacement = InsideWord SongChord | BetweenWord SongChord

data SongSectionName = Intro | Chorus | Verse | Outro | Bridge


--- Song parser

parseNewline :: Parser String
parseNewline = string "\r\n" <|> string "\n"

parseCarriageReturn :: Parser String
parseCarriageReturn = try $ do
    parseNewline
    parseNewline

parseSongMeta :: Parser SongMeta
parseSongMeta = do
    title <- untilNewline anyChar
    artist <- untilNewline anyChar
    year <- untilNewline anyDigit
    album <- untilNewline anyChar
    pure $ SongMeta {
        title: charsToString title,
        artist: charsToString artist,
        year: Year $ fromMaybe 0 (fromString $ charsToString year),
        album: charsToString album
    }
    where charsToString = fromCharArray <<< fromFoldable
          untilNewline p = manyTill p parseNewline

serializeSongMeta :: SongMeta -> String
serializeSongMeta (SongMeta { title, artist, year: Year y, album })
    = title <> "\n" <>
      artist <> "\n" <>
      show y <> "\n" <>
      album <> "\n"

parseSongSectionName :: Parser SongSectionName
parseSongSectionName =
        string "Intro" $> Intro
    <|> string "Chorus" $> Chorus
    <|> string "Verse" $> Verse
    <|> string "Outro" $> Outro
    <|> string "Bridge" $> Bridge

serializeSongSectionName :: SongSectionName -> String
serializeSongSectionName Intro = "Intro"
serializeSongSectionName Chorus = "Chorus"
serializeSongSectionName Verse = "Verse"
serializeSongSectionName Outro = "Outro"
serializeSongSectionName Bridge = "Bridge"

parseSongSection :: Parser SongSection
parseSongSection = do
    name <- parseSongSectionName
    parseNewline
    lyrics <- manyTill (parseSongLyric end) parseCarriageReturn
    pure $ SongSection { name, lyrics: fromFoldable lyrics }
        where end = lookAhead (string "/") <|> lookAhead parseCarriageReturn <|> lookAhead (string "\\") <|> lookAhead (string "|")

serializeSongSection :: SongSection -> String
serializeSongSection (SongSection { name, lyrics }) =
    serializeSongSectionName name <> "\n" <>
    joinWith "" (serializeSongLyric <$> lyrics) <> "\n\n"

parseSongLyric :: forall a. Parser a -> Parser SongLyric
parseSongLyric end = parseOnlyLyric end <|> parseOnlyChord end <|> parseChordAndLyric end

parseChordAndLyric :: forall a. Parser a -> Parser SongLyric
parseChordAndLyric end = do
    cp <- parseChordPlacement
    lyric <- manyTill anyChar end
    pure $ ChordAndLyric cp $ parseLyric lyric

parseChordPlacement :: Parser ChordPlacement
parseChordPlacement = try parseBetweenWord <|> parseInsideWord
    where
        parseBetweenWord = do
            string "/"
            chord <- parseChord
            string "/ "
            pure $ BetweenWord chord

        parseInsideWord = do
            string "/"
            chord <- parseChord
            string "\\"
            pure $ InsideWord chord


parseOnlyLyric :: forall a. Parser a -> Parser SongLyric
parseOnlyLyric end = do
    string "| "
    lyric <- manyTill anyChar end
    pure $ OnlyLyric $ parseLyric lyric

parseOnlyChord :: forall a. Parser a -> Parser SongLyric
parseOnlyChord end = do
    string "\\"
    chord <- parseChord
    string "\\ "
    pure $ OnlyChord chord

parseLyric :: List Char -> String
parseLyric l = fromCharArray <<< fromFoldable $ l

serializeSongLyric :: SongLyric -> String
serializeSongLyric (ChordAndLyric cp lyric) = serializeChordPlacement cp <> lyric
serializeSongLyric (OnlyLyric lyric) = "| " <> lyric
serializeSongLyric (OnlyChord chord) = "\\" <> show chord <> "\\ "

serializeChordPlacement :: ChordPlacement -> String
serializeChordPlacement (BetweenWord chord) = "/" <> show chord <> "/ "
serializeChordPlacement (InsideWord chord) = "/" <> show chord <> "\\"


parseSongContent :: Parser SongContent
parseSongContent = do
    content <- manyTill parseSongSection eof
    pure $ SongContent $ fromFoldable content

serializeSongContent :: SongContent -> String
serializeSongContent (SongContent s) = joinWith "" $ serializeSongSection <$> s

parseSong :: Parser Song
parseSong = do
    meta <- parseSongMeta
    parseNewline
    content <- parseSongContent
    pure $ Song { meta, content }

serializeSong :: Song -> String
serializeSong (Song { meta, content }) =
    serializeSongMeta meta
    <> "\n" <>
    serializeSongContent content


--- Generic boilerplate

derive instance genericDBSong :: Generic DBSong

instance isForeignDBSong :: IsForeign DBSong where
    read = readGeneric defaultOptions

derive instance genericSong :: Generic Song

instance showSong :: Show Song where
    show = gShow

instance arbitrarySong :: Arbitrary Song where
    arbitrary = gArbitrary

instance eqSong :: Eq Song where
    eq = gEq

instance isForeignSong :: IsForeign Song where
    read = readGeneric defaultOptions

derive instance genericSongMeta :: Generic SongMeta

instance arbitrarySongMeta :: Arbitrary SongMeta where
    arbitrary = gArbitrary

instance showSongMeta :: Show SongMeta where
    show = gShow

instance eqSongMeta :: Eq SongMeta where
    eq = gEq

instance isForeignSongMeta :: IsForeign SongMeta where
    read = readGeneric defaultOptions

derive instance genericYear :: Generic Year

instance arbYear :: Arbitrary Year where
    arbitrary = gArbitrary

instance eqYear :: Eq Year where
    eq = gEq

instance isForeignYear :: IsForeign Year where
    read = readGeneric defaultOptions

derive instance genericSongContent :: Generic SongContent

instance arbitrarySongContent :: Arbitrary SongContent where
    arbitrary = gArbitrary

instance eqSongContent :: Eq SongContent where
    eq = gEq

instance showSongContent :: Show SongContent where
    show = gShow

instance isForeignSongContent :: IsForeign SongContent where
    read = readGeneric defaultOptions

derive instance genericSongSection :: Generic SongSection

instance isForeignSongSection :: IsForeign SongSection where
    read = readGeneric defaultOptions

instance arbitrarySongSection :: Arbitrary SongSection where
    arbitrary = gArbitrary

instance eqSongSection :: Eq SongSection where
    eq = gEq

instance showSongSection :: Show SongSection where
    show = gShow

derive instance genericSongLyric :: Generic SongLyric

instance arbitrarySongLyric :: Arbitrary SongLyric where
    arbitrary = gArbitrary

instance showSongLyric :: Show SongLyric where
    show = gShow

instance eqSongLyric :: Eq SongLyric where
    eq = gEq

instance isForeignSongLyric :: IsForeign SongLyric where
    read = readGeneric defaultOptions

derive instance genericSongSectionName :: Generic SongSectionName

instance isForeignSongSectionName :: IsForeign SongSectionName where
    read = readGeneric defaultOptions

derive instance genericChordPlacment :: Generic ChordPlacement

instance showChordPlacment :: Show ChordPlacement where
    show = gShow


--- Test data

exampleSong :: Song
exampleSong = Song {
    meta: exampleSongMeta,
    content: exampleSongContent
}

exampleSongMeta :: SongMeta
exampleSongMeta = SongMeta {
    title: "Tokyo vampires and wolves",
    artist: "The Wombats",
    album: "This modern glitch",
    year: Year 2011
}

exampleSongContent :: SongContent
exampleSongContent = SongContent [ SongSection {
    name: Intro,
    lyrics: [ ChordAndLyric (BetweenWord exampleChord) "test" ]
}]
