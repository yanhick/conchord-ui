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
getSongMetaType s
  | (filter (\x -> x == s) getStandardSongMetaType) /= [] = Other s
  | otherwise = Other s

getMatchingSongMeta :: String -> [String]
getMatchingSongMeta s = filter (==s) getStandardSongMetaType

getStandardSongMetaType :: [String]
getStandardSongMetaType = fmap (\x -> fmap toUpper x) (fmap show [Title .. Album])

parseMeta :: String -> Maybe SongMeta
parseMeta s =
  case words s of
    [k, v] -> Just (SongMeta (getSongMetaType k) v)
    _ -> Nothing

