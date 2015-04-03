module SongMetaParser where

import Data.Char
import Control.Applicative

data SongMetaType
  = Standard StandardSongMetaType
  | Other String
  deriving (Show, Eq)

data StandardSongMetaType
  = Title
  | Artist
  | Album
  deriving (Show, Enum, Read, Eq)

type SongMetaParseError = String

type SongMetaValue = String
data SongMeta = SongMeta (SongMetaType, SongMetaValue) deriving (Show, Eq)

getSongMetaType :: String -> SongMetaType
getSongMetaType s =
  case getStandardSongMetaType s of
    Just x -> Standard x
    Nothing -> Other s

getStandardSongMetaType :: String -> Maybe StandardSongMetaType
getStandardSongMetaType s =
  case fmap toLower s of
    "title" -> Just Title
    "artist" -> Just Artist
    "album" -> Just Album
    _ -> Nothing

parseMeta :: String -> Either SongMetaParseError SongMeta
parseMeta s =
  case words s of
    [k, v] -> Right (SongMeta (getSongMetaType k, v))
    _ -> Left "wrong number of args"


parseSongMeta:: String -> [Either SongMetaParseError SongMeta]
parseSongMeta l = parseMeta <$> lines l
