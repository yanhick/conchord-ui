module Model where

import Prelude (pure, class Show, bind, ($), (<>), (<$>))

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Null (runNull)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing, Just))

import Route (Route(HomePage))


--- App State

type State = {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

type UIState = {
    songFontSize :: Number
  , searchQuery :: String
}

type IOState = {
    searchResults :: SearchResults
  , song :: Maybe Song
}

init :: State
init = {
    currentPage: HomePage
  , ui: {
      songFontSize: 12.0
    , searchQuery: ""
  }
  , io: {
      searchResults: []
    , song: Nothing
  }
}

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

type Song = {
    id :: Int,
    meta :: SongMeta,
    content :: SongContent
}

type SongMeta = {
    title :: String,
    artist :: String,
    album :: Maybe String,
    year :: Year
}

type Year = Int

type SongContent = Array SongSection

type SongSection = {
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

newtype SongLyric = SongLyric {
    lyric :: Maybe String,
    chord :: SongChord
}

instance isForeignSongLyric :: IsForeign SongLyric where
    read value = do
        lyric <- runNull <$> readProp "lyric" value
        chord <- readProp "chord" value
        pure $ SongLyric {lyric, chord}

data SongChord = A | B | C | D | E | F | G | Am | Bm | Cm | Dm | Em | Fm | Gm

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
toChord s = Left $ TypeMismatch "Expected Valid Chord" ("Found " <> s)

data SongSectionName = Intro | Chorus | Verse | Outro | Bridge

instance showSongSectionName :: Show SongSectionName where
    show Intro = "Intro"
    show Chorus = "Chorus"
    show Verse = "Verse"
    show Outro = "Outro"
    show Bridge = "Bridge"

--- Song Example

song :: Song
song = {
    id: 1,
    meta: {
        title: "Tokyo vampires and wolves",
        artist: "The Wombats",
        album: Just "This modern glitch",
        year: 2011
    },
    content: [{
        name: Verse,
        lyrics: [SongLyric {
            lyric: Just "We're self imploding,",
            chord: Am
        }, SongLyric {
            lyric: Nothing,
            chord: C
        }, SongLyric {
            lyric: Nothing,
            chord: Dm
        }, SongLyric {
            lyric: Just "under",
            chord: C
        }, SongLyric {
            lyric: Just "the weight of",
            chord: Dm
        }, SongLyric {
            lyric: Just "your",
            chord: Em
        }, SongLyric {
            lyric: Just "advice.",
            chord: F
        }]
    }]
}
