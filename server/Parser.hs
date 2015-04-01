import Data.Char

data SongMetaType
  = Standard StandardSongMetaType
  | Other String
  deriving Show

data StandardSongMetaType
  = Title
  | Artist
  | Album
  deriving (Show, Enum, Read)

data SongMeta = SongMeta {
  key :: SongMetaType
, value :: String
} deriving Show

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
    [k, v] -> Just (SongMeta (getSongMetaType k) v)
    _ -> Nothing

