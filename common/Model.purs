module Model where

import Prelude (pure, class Show, bind, ($), (<$>))

import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Null (runNull)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing, Just))

--- Search Model

newtype SearchResult = SearchResult { id :: Int, title :: String, desc :: String }

instance isForeignSearchResult :: IsForeign SearchResult where
    read value = do
        id <- readProp "id" value
        title <- readProp "title" value
        desc <- readProp "desc" value
        pure $ SearchResult { id, title, desc }

type SearchResults = Array SearchResult


--- Song Model

newtype Song = Song {
    id :: Int,
    meta :: SongMeta,
    content :: SongContent
}

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

newtype Album = Album (Maybe String)

instance isForeignSongMeta :: IsForeign SongMeta where
    read value = do
        title <- readProp "title" value
        artist <- readProp "artist" value
        album <- runNull <$> readProp "album" value
        year <- readProp "year" value
        pure $ SongMeta { title, artist, album, year }

newtype Year = Year Int

instance isForeignYear :: IsForeign Year where
    read value = do
        y <- read value
        pure $ Year y

newtype SongContent = SongContent (Array SongSection)

instance isForeignSongContent :: IsForeign SongContent where
    read value = do
        c <- read value
        pure $ SongContent c

newtype SongSection = SongSection {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

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
        lyric <- runNull <$> readProp "lyric" value
        chord <- runNull <$> readProp "chord" value
        pure $ SongLyric { lyric, chord }

data SongChord = A | B | C | D | E | F | G | Am | Bm | Cm | Dm | Em | Fm | Gm | Am7 | G7

instance showSongChord :: Show SongChord where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"
    show G = "G"
    show Am = "Am"
    show Bm = "Bm"
    show Cm = "Cm"
    show Dm = "Dm"
    show Em = "Em"
    show Fm = "Fm"
    show Gm = "Gm"
    show Am7 = "Am7"
    show G7 = "G7"

instance isForeignSongChord :: IsForeign SongChord where
    read value = do
        s <- readString value
        toChord s

toChord :: String -> F SongChord
toChord "A" = pure A
toChord "B" = pure B
toChord "C" = pure C
toChord "D" = pure D
toChord "E" = pure E
toChord "F" = pure F
toChord "G" = pure G
toChord "Am" = pure Am
toChord "Bm" = pure Bm
toChord "Cm" = pure Cm
toChord "Dm" = pure Dm
toChord "Em" = pure Em
toChord "Fm" = pure Fm
toChord "Gm" = pure Gm
toChord "Am7" = pure Am7
toChord "G7" = pure G7
toChord s = Left $ TypeMismatch "Valid Chord" s

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


--- Song Example

song :: Song
song = Song {
    id: 1,
    meta: SongMeta {
        title: "Tokyo vampires and wolves",
        artist: "The Wombats",
        album: Just "This modern glitch",
        year: Year 2011
    },
    content: SongContent [ SongSection {
        name: Verse,
        lyrics: [ SongLyric {
            lyric: Just "We're self imploding,",
            chord: Just Am
        }, SongLyric {
            lyric: Nothing,
            chord: Just C
        }, SongLyric {
            lyric: Nothing,
            chord: Just Dm
        }, SongLyric {
            lyric: Just "under",
            chord: Just C
        }, SongLyric {
            lyric: Just "the weight of",
            chord: Just Dm
        }, SongLyric {
            lyric: Just "your",
            chord: Just Em
        }, SongLyric {
            lyric: Just "advice.",
            chord: Just F
        }]
    }]
}

songJSON :: String
songJSON = """{ "id": 1, "meta": { "title": "Tokyo vampires and wolves", "artist": "The Wombats", "album": "This modern glitch", "year": 2011 }, "content": [ { "name": "Verse", "lyrics": [ { "lyric": "We're self imploding,", "chord": "Am" } ] }] }"""
