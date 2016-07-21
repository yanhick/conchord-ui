module Model where

import Prelude (pure, class Show, bind, ($), (<$>), show, class Eq, (==), (&&))

import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Null (unNull)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just))
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions, toJSONGeneric)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Parser (SongChord, exampleChord)

--- Search Model

newtype SearchResult = SearchResult { id :: Int, meta :: SongMeta, desc :: String }

derive instance genericSearchResult :: Generic SearchResult

instance isForeignSearchResult :: IsForeign SearchResult where
    read = readGeneric defaultOptions

type SearchResults = Array SearchResult



--- Song Model

newtype Song = Song {
    id :: Int,
    meta :: SongMeta,
    content :: SongContent
}

derive instance genericSong :: Generic Song


instance isForeignSong :: IsForeign Song where
    read = readGeneric defaultOptions

newtype SongMeta = SongMeta {
    title :: String,
    artist :: String,
    album :: Maybe String,
    year :: Year
}

derive instance genericSongMeta :: Generic SongMeta

instance eqSongMeta :: Eq SongMeta where
    eq = gEq

instance arbSongMeta :: Arbitrary SongMeta where
    arbitrary = do
        title <- arbitrary
        artist <- arbitrary
        album <- arbitrary
        year <- arbitrary
        pure $ SongMeta {
            title: title,
            artist: artist,
            album: album,
            year: year
        }


newtype Album = Album (Maybe String)

instance isForeignSongMeta :: IsForeign SongMeta where
    read = readGeneric defaultOptions

newtype Year = Year Int

derive instance genericYear :: Generic Year

instance arbYear :: Arbitrary Year where
    arbitrary = Year <$> arbitrary

instance eqYear :: Eq Year where
    eq = gEq

instance isForeignYear :: IsForeign Year where
    read = readGeneric defaultOptions

newtype SongContent = SongContent (Array SongSection)

derive instance genericSongContent :: Generic SongContent


instance isForeignSongContent :: IsForeign SongContent where
    read = readGeneric defaultOptions

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

derive instance genericSongSection :: Generic SongSection


instance isForeignSongSection :: IsForeign SongSection where
    read = readGeneric defaultOptions

newtype SongLyric = SongLyric {
    lyric :: Maybe String,
    chord :: Maybe SongChord
}

derive instance genericSongLyric :: Generic SongLyric

instance isForeignSongLyric :: IsForeign SongLyric where
    read = readGeneric defaultOptions


data SongSectionName = Intro | Chorus | Verse | Outro | Bridge

derive instance genericSongSectionName :: Generic SongSectionName

instance showSongSectionName :: Show SongSectionName where
    show = gShow

instance isForeignSongSectionName :: IsForeign SongSectionName where
    read = readGeneric defaultOptions

exampleSong :: Song
exampleSong = Song{
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
