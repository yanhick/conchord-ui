module Model where

import Prelude (pure, class Show, bind, ($), (<$>), show, class Eq, (==), (&&))

import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Null (unNull)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Maybe (Maybe())
import Data.Argonaut (class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, fromArray)
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions, toJSONGeneric)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Parser (SongChord)

--- Search Model

newtype SearchResult = SearchResult { id :: Int, meta :: SongMeta, desc :: String }

derive instance genericSearchResult :: Generic SearchResult

instance isForeignSearchResult :: IsForeign SearchResult where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

type SearchResults = Array SearchResult

instance encodeJsonSearchResult :: EncodeJson SearchResult where
    encodeJson (SearchResult { id, meta, desc })
        = "id" := id
        ~> "meta" := encodeJson meta
        ~> "desc" := desc
        ~> jsonEmptyObject


--- Song Model

newtype Song = Song {
    id :: Int,
    meta :: SongMeta,
    content :: SongContent
}

derive instance genericSong :: Generic Song

instance encodeJsonSong :: EncodeJson Song where
    encodeJson (Song { id, meta, content })
        = "id" := id
        ~> "meta" := encodeJson meta
        ~> "content" := encodeJson content
        ~> jsonEmptyObject

instance isForeignSong :: IsForeign Song where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

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

instance encodeJsonSongMeta :: EncodeJson SongMeta where
    encodeJson (SongMeta { title, artist, album, year: Year(y) })
        = "title" := title
        ~> "artist" := artist
        ~> "album" := album
        ~> "year" := y
        ~> jsonEmptyObject

newtype Album = Album (Maybe String)

instance isForeignSongMeta :: IsForeign SongMeta where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

newtype Year = Year Int

derive instance genericYear :: Generic Year

instance arbYear :: Arbitrary Year where
    arbitrary = Year <$> arbitrary

instance eqYear :: Eq Year where
    eq = gEq

instance isForeignYear :: IsForeign Year where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

newtype SongContent = SongContent (Array SongSection)

derive instance genericSongContent :: Generic SongContent

instance encodeJsonSongContent :: EncodeJson SongContent where
    encodeJson (SongContent s) = fromArray $ encodeJson <$> s

instance isForeignSongContent :: IsForeign SongContent where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

derive instance genericSongSection :: Generic SongSection

instance encodeJsonSongSection :: EncodeJson SongSection where
    encodeJson (SongSection { name, lyrics })
        = "name" := toJSONGeneric defaultOptions { unwrapNewtypes = true } name
        ~> "lyrics" := encodeJson lyrics
        ~> jsonEmptyObject

instance isForeignSongSection :: IsForeign SongSection where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

newtype SongLyric = SongLyric {
    lyric :: Maybe String,
    chord :: Maybe SongChord
}

derive instance genericSongLyric :: Generic SongLyric

instance isForeignSongLyric :: IsForeign SongLyric where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

instance encodeJsonSongLyric :: EncodeJson SongLyric where
    encodeJson (SongLyric { lyric, chord })
        = "lyric" := lyric
        ~> "chord" := (show <$> chord)
        ~> jsonEmptyObject


data SongSectionName = Intro | Chorus | Verse | Outro | Bridge

derive instance genericSongSectionName :: Generic SongSectionName

instance showSongSectionName :: Show SongSectionName where
    show = gShow

instance isForeignSongSectionName :: IsForeign SongSectionName where
    read = readGeneric defaultOptions
