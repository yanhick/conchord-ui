module Model where

import Prelude

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Maybe (Maybe(Nothing, Just))

newtype Result = Result { id :: Int, title :: String, desc :: String }

instance resultIsForeign :: IsForeign Result where
    read value = do
        id <- readProp "id" value
        title <- readProp "title" value
        desc <- readProp "desc" value
        return $ Result { id, title, desc }

type List = Array Result

type Search = { q :: String }

type Detail = Result


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
    id :: Int,
    name :: SongSectionName,
    lyrics :: Array SongLyric
}

type SongLyric = {
    id :: Int,
    text :: Maybe String,
    chord :: SongChord
}

data SongChord = A | B | C | D | E | F | G | Am | Bm | Cm | Dm | Em | Fm | Gm

data SongSectionName = Intro | Chorus | Verse | Outro | Bridge

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
        id: 1,
        name: Verse,
        lyrics: [{
            id: 1,
            text: Just "We're self imploding,",
            chord: Am
        }, {
            id: 2,
            text: Nothing,
            chord: C
        }, {
            id: 3,
            text: Nothing,
            chord: Dm
        }, {
            id: 4,
            text: Just "under",
            chord: C
        }, {
            id: 5,
            text: Just "the weight of",
            chord: Dm
        }, {
            id: 6,
            text: Just "your",
            chord: Em
        }, {
            id: 7,
            text: Just "advice.",
            chord: F
        }]
    }]
}
