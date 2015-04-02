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

data ChordName = ChordName String deriving Show

data ChordNote
  = Pressed Fret
  | Muted
  | Opened
  deriving Show

data ChordNotes = ChordNotes {
  highE :: ChordNote
, b :: ChordNote
, g :: ChordNote
, d :: ChordNote
, a :: ChordNote
, lowE :: ChordNote
} deriving Show

data Fingering = Fingering String deriving Show
data FingeringList = FingeringList [Fingering] deriving Show

data Chord = Chord {
  name :: ChordName
, notes :: Maybe ChordNotes
, fingerings :: Maybe FingeringList
, baseFret :: Maybe Fret
} deriving Show

{-- TODO: go to 22, implement Read, check what is Bounded --}
data Fret
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Show, Eq, Ord, Enum)

instance Read Fret where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]
  readsPrec _ "3" = [(Three, "")]
  readsPrec _ "4" = [(Four, "")]
  readsPrec _ "5" = [(Five, "")]
  readsPrec _ "6" = [(Six, "")]
  readsPrec _ _  = []

parseChordName :: String -> ChordName
parseChordName = ChordName

parseChordNotes :: String -> Maybe ChordNotes
parseChordNotes ns =
  case ns of
    [lE, a, d, g, b, hE] -> Just $ ChordNotes (parseNote lE)
                                              (parseNote a)
                                              (parseNote d)
                                              (parseNote g)
                                              (parseNote b)
                                              (parseNote hE)
    _ -> Nothing
  where parseNote '-' = Opened
        parseNote 'x' = Muted
        {--TODO: manage parse error (unknow char, return Maybe ?) --}
        parseNote n = Pressed $ read [n]

parseFingeringList :: String -> FingeringList
parseFingeringList = undefined

parseChord :: String -> Maybe Chord
parseChord s =
  case words s of
    [chordName, chordNotes] -> Just Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = Nothing
                  , baseFret = Nothing
                     }
    [chordName, chordNotes, chordFingerings] -> Just Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = Just $ parseFingeringList chordFingerings
                  , baseFret = Nothing
                  }

    [chordName, chordNotes, chordFingerings, chordFret] -> Just Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = Just $ parseFingeringList chordFingerings
                  , baseFret = Just $ read chordFret
                  }
    _ -> Nothing
