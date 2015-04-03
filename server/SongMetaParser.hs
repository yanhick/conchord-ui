module SongMetaParserString
(
  parseSongMeta
) where

import Data.Char
import Data.Maybe
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
    "Album" -> Just Album
    _ -> Nothing

parseMeta :: String -> Maybe SongMeta
parseMeta s =
  case words s of
    [k, v] -> Just (SongMeta (getSongMetaType k, v))
    _ -> Nothing


parseSongMeta:: String -> [Maybe SongMeta]
parseSongMeta l = filter isJust (parseMeta <$> lines l)
