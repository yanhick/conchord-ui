module SongMetaParser where

import Control.Applicative
import qualified Data.Text as T

data SongMetaType
  = Standard StandardSongMetaType
  | Other T.Text
  deriving (Show, Eq)

data StandardSongMetaType
  = Title
  | Artist
  | Album
  deriving (Show, Enum, Read, Eq)

type SongMetaParseError = String

type SongMetaValue = T.Text
data SongMeta = SongMeta (SongMetaType, SongMetaValue) deriving (Show, Eq)

getSongMetaType :: T.Text -> SongMetaType
getSongMetaType s =
  case getStandardSongMetaType s of
    Just x -> Standard x
    Nothing -> Other s

getStandardSongMetaType :: T.Text -> Maybe StandardSongMetaType
getStandardSongMetaType s
  | match s "title" = Just Title
  | match s "artist" = Just Artist
  | match s "album" = Just Album
  | otherwise = Nothing
  where match s n = T.toLower s == (T.pack n)

parseMeta :: T.Text -> Either SongMetaParseError SongMeta
parseMeta s =
  case T.words s of
    [k, v] -> Right (SongMeta (getSongMetaType k, v))
    _ -> Left "wrong number of args"


parseSongMeta:: T.Text -> [Either SongMetaParseError SongMeta]
parseSongMeta l = parseMeta <$> T.lines l
