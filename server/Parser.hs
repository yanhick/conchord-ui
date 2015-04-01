import Data.Char

{-- Parse song metas --}

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

parseMetaList :: String -> [Maybe SongMeta]
parseMetaList l = fmap parseMeta $ lines l

{-- Parse song chords --}

data ChordName = ChordName String

data ChordNote
  = Pressed Int
  | Muted
  | Opened

data ChordNotes = ChordNotes {
  highE :: ChordNote
, b :: ChordNote
, g :: ChordNote
, d :: ChordNote
, a :: ChordNote
, lowE :: ChordNote
}

data Fingering = Fingering String
data FingeringList = FingeringList [Fingering]

data Chord = Chord {
  name :: ChordName
, notes :: ChordNotes
, fingerings :: Maybe FingeringList
, fret :: Maybe Int
}

parseChordName :: String -> ChordName
parseChordName = undefined

parseChordNotes :: String -> ChordNotes
parseChordNotes = undefined

parseFingeringList :: String -> FingeringList
parseFingeringList = undefined

parseChord :: String -> Chord
parseChord = undefined
