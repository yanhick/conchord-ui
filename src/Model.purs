module Model where

import Prelude

import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign (readInt, readArray, F(), ForeignError(TypeMismatch), Foreign())
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Either (Either(Left, Right), either)

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
    lines :: Array SongLyric
}

type SongLyric = {
    id :: Int,
    text :: String,
    chord :: SongChord
}

data SongChord = A | B | C | D | E | F | G

data SongSectionName = Intro | Chorus | Verse | Outro | Bridge
