module Model where

import Prelude (pure, class Show, bind, ($), (<$>), class Eq, (==), (&&), (<<<))

import Data.Foreign.Class (class IsForeign)
import Data.String (fromCharArray)
import Data.Array (fromFoldable)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions)

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Generic (gArbitrary)

import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (string, eof, anyChar, anyDigit)
import Text.Parsing.StringParser.Combinators (optionMaybe, manyTill)

import Parser (SongChord, exampleChord)

--- Search Model

newtype SearchResult = SearchResult { id :: Int, meta :: SongMeta, desc :: String }

type SearchResults = Array SearchResult

--- Song Model

newtype Song = Song {
    id :: Int,
    meta :: SongMeta,
    content :: SongContent
}

newtype SongMeta = SongMeta {
    title :: String,
    artist :: String,
    album :: Maybe String,
    year :: Year
}

newtype Album = Album (Maybe String)

newtype Year = Year Int

newtype SongContent = SongContent (Array SongSection)

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

newtype SongLyric = SongLyric {
    lyric :: Maybe String,
    chord :: Maybe SongChord
}

data SongSectionName = Intro | Chorus | Verse | Outro | Bridge


--- Generic boilerplate

derive instance genericSearchResult :: Generic SearchResult

instance isForeignSearchResult :: IsForeign SearchResult where
    read = readGeneric defaultOptions

derive instance genericSong :: Generic Song

instance arbitrarySong :: Arbitrary Song where
    arbitrary = gArbitrary

instance showSong :: Show Song where
    show = gShow

instance eqSong :: Eq Song where
    eq = gEq

instance isForeignSong :: IsForeign Song where
    read = readGeneric defaultOptions

derive instance genericSongMeta :: Generic SongMeta

instance eqSongMeta :: Eq SongMeta where
    eq = gEq

instance showSongMeta  :: Show SongMeta where
    show = gShow

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

instance isForeignSongContent :: IsForeign SongContent where
    read = readGeneric defaultOptions

derive instance genericSongSection :: Generic SongSection

instance isForeignSongSection :: IsForeign SongSection where
    read = readGeneric defaultOptions

derive instance genericSongLyric :: Generic SongLyric

instance isForeignSongLyric :: IsForeign SongLyric where
    read = readGeneric defaultOptions

derive instance genericSongSectionName :: Generic SongSectionName

instance showSongSectionName :: Show SongSectionName where
    show = gShow

instance isForeignSongSectionName :: IsForeign SongSectionName where
    read = readGeneric defaultOptions

--- Song parser

parseNewline :: Parser String
parseNewline = string "\n"

parseSongMeta :: Parser SongMeta
parseSongMeta = do
    title <- untilNewline anyChar
    artist <- untilNewline anyChar
    year <- untilNewline anyDigit
    album <- optionMaybe $ manyTill anyChar parseNewline
    pure $ SongMeta {
        title: charsToString title,
        artist: charsToString artist,
        year: Year $ fromMaybe 0 (fromString $ charsToString year),
        album: fromCharArray <<< fromFoldable <$> album
    }
    where charsToString = fromCharArray <<< fromFoldable
          untilNewline p = manyTill p parseNewline
 
--- Test data

exampleSong :: Song
exampleSong = Song {
    id: 1,
    meta: exampleSongMeta,
    content: exampleSongContent
}

exampleSongMeta :: SongMeta
exampleSongMeta = SongMeta {
    title: "Tokyo vampires and wolves",
    artist: "The Wombats",
    album: Just "This modern glitch",
    year: Year 2011
}

exampleSongContent :: SongContent
exampleSongContent = SongContent [ SongSection {
    name: Intro,
    lyrics: [ SongLyric {
        chord: Just exampleChord,
        lyric: Just "test"
    }]
}]
