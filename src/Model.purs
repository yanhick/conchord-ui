module Model where

import Prelude (pure, class Show, bind, ($), (<$>), show, class Eq, (==), (&&))

import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Null (unNull)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Maybe (Maybe())
import Data.Argonaut (class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, fromArray)
import Data.Generic (class Generic, gEq)
import Data.Foreign.Generic (readGeneric, defaultOptions)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Parser (SongChord)

--- Search Model

newtype SearchResult = SearchResult { id :: Int, meta :: SongMeta, desc :: String }

instance isForeignSearchResult :: IsForeign SearchResult where
    read value = do
        id <- readProp "id" value
        meta <- readProp "meta" value
        desc <- readProp "desc" value
        pure $ SearchResult { id, meta, desc }

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

instance encodeJsonSong :: EncodeJson Song where
    encodeJson (Song { id, meta, content })
        = "id" := id
        ~> "meta" := encodeJson meta
        ~> "content" := encodeJson content
        ~> jsonEmptyObject

instance isForeignSong :: IsForeign Song where
    read value = do
        id <- readProp "id" value
        meta <- readProp "meta" value
        content <- readProp "content" value
        pure $ Song { id, meta, content }

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
    read value = do
        title <- readProp "title" value
        artist <- readProp "artist" value
        album <- unNull <$> readProp "album" value
        year <- readProp "year" value
        pure $ SongMeta { title, artist, album, year }

newtype Year = Year Int

derive instance genericYear :: Generic Year

instance arbYear :: Arbitrary Year where
    arbitrary = Year <$> arbitrary

instance eqYear :: Eq Year where
    eq = gEq

instance isForeignYear :: IsForeign Year where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

newtype SongContent = SongContent (Array SongSection)

instance encodeJsonSongContent :: EncodeJson SongContent where
    encodeJson (SongContent s) = fromArray $ encodeJson <$> s

instance isForeignSongContent :: IsForeign SongContent where
    read value = do
        c <- read value
        pure $ SongContent c

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

instance encodeJsonSongSection :: EncodeJson SongSection where
    encodeJson (SongSection { name, lyrics })
        = "name" := show name
        ~> "lyrics" := encodeJson lyrics
        ~> jsonEmptyObject

instance isForeignSongSection :: IsForeign SongSection where
    read value = do
        name <- readProp "name" value
        lyrics <- readProp "lyrics" value
        pure $ SongSection { name, lyrics }

newtype SongLyric = SongLyric {
    lyric :: Maybe String,
    chord :: Maybe SongChord
}

instance isForeignSongLyric :: IsForeign SongLyric where
    read value = do
        lyric <- unNull <$> readProp "lyric" value
        chord <- unNull <$> readProp "chord" value
        pure $ SongLyric { lyric, chord }

instance encodeJsonSongLyric :: EncodeJson SongLyric where
    encodeJson (SongLyric { lyric, chord })
        = "lyric" := lyric
        ~> "chord" := (show <$> chord)
        ~> jsonEmptyObject


data SongSectionName = Intro | Chorus | Verse | Outro | Bridge

instance showSongSectionName :: Show SongSectionName where
    show Intro = "Intro"
    show Chorus = "Chorus"
    show Verse = "Verse"
    show Outro = "Outro"
    show Bridge = "Bridge"

instance isForeignSongSectionName :: IsForeign SongSectionName where
    read value = do
        s <- readString value
        toSongSectionName s

toSongSectionName :: String -> F SongSectionName
toSongSectionName "Intro" = pure Intro
toSongSectionName "Chorus" = pure Chorus
toSongSectionName "Verse" = pure Verse
toSongSectionName "Outro" = pure Outro
toSongSectionName "Bridge" = pure Bridge
toSongSectionName s = Left $ TypeMismatch "Valid Song Section Name" s
