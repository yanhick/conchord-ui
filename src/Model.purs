module Model where

import Prelude (pure, class Show, bind, ($), (<>))

import Data.Foreign.Class (class IsForeign, readProp)
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

type SongLyric = {
    text :: Maybe String,
    chord :: SongChord
}

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
        chord <- readString value
        case chord of
          "A" -> pure A
          _ -> error chord

        where

        error :: String -> F SongChord
        error chord = Left $ TypeMismatch "Expected Chord" ("Found " <> chord)

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
        lyrics: [{
            text: Just "We're self imploding,",
            chord: Am
        }, {
            text: Nothing,
            chord: C
        }, {
            text: Nothing,
            chord: Dm
        }, {
            text: Just "under",
            chord: C
        }, {
            text: Just "the weight of",
            chord: Dm
        }, {
            text: Just "your",
            chord: Em
        }, {
            text: Just "advice.",
            chord: F
        }]
    }]
}
