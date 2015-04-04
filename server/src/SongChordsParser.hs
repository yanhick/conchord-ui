module SongChordsParser where

import Control.Applicative

{- refine (major, minor...) -}
data ChordName = ChordName String deriving Show

data ChordNote
  = CPressed Fret
  | CMuted
  | COpened
  | CNotPlayed
  deriving Show

type ChordNotes = GuitarStrings ChordNote

type ChordFingerings = GuitarStrings Fingering

data Finger
  = Thumb
  | Index
  | Middle
  | Ring
  | Pinky
  deriving (Show, Enum)

instance Read Finger where
  readsPrec _ "1" = [(Thumb, "")]
  readsPrec _ "2" = [(Index, "")]
  readsPrec _ "3" = [(Middle, "")]
  readsPrec _ "4" = [(Ring, "")]
  readsPrec _ "5" = [(Pinky, "")]
  readsPrec _ _  = []

data Fingering
  = FPressed Finger
  | FOpened
  deriving Show

data GuitarStrings a = GuitarStrings {
  highE :: a
, b :: a
, g :: a
, d :: a
, a :: a
, lowE :: a
} deriving Show

data Chord = Chord {
  name :: ChordName
, baseFret :: Fret
, notes :: Either ChordParserError ChordNotes
, fingerings :: Maybe ChordFingerings
} deriving Show

{-- TODO: go to 22, implement Read, check what is Bounded --}
data Fret
  = FZero
  | FOne
  | FTwo
  | FThree
  | FFour
  | FFive
  | FSix
  deriving (Show, Eq, Ord, Enum)


instance Read Fret where
  readsPrec _ "1" = [(FOne, "")]
  readsPrec _ "2" = [(FTwo, "")]
  readsPrec _ "3" = [(FThree, "")]
  readsPrec _ "4" = [(FFour, "")]
  readsPrec _ "5" = [(FFive, "")]
  readsPrec _ "6" = [(FSix, "")]
  readsPrec _ _  = []

parseChordName :: String -> ChordName
parseChordName = ChordName

parseChordNotes :: String -> Either ChordParserError ChordNotes
parseChordNotes ns =
  case ns of
    [lE, a, d, g, b, hE] -> Right $ GuitarStrings (parseNote lE)
                                              (parseNote a)
                                              (parseNote d)
                                              (parseNote g)
                                              (parseNote b)
                                              (parseNote hE)
    _ -> Left "wrong number of chord notes"
  where parseNote '-' = COpened
        parseNote 'x' = CMuted
        {--TODO: manage parse error (unknow char, return Maybe ?) --}
        parseNote n = CPressed $ read [n]

parseChordFingerings:: String -> Maybe ChordFingerings
parseChordFingerings fs =
  case fs of
    [lE, a, d, g, b, hE] -> Just $ GuitarStrings (parseFingering lE)
                                                (parseFingering a)
                                                (parseFingering d)
                                                (parseFingering g)
                                                (parseFingering b)
                                                (parseFingering hE)
    _ -> Nothing
  where parseFingering '-' = FOpened
        parseFingering f = FPressed $ read [f]

type ChordParserError = String

parseChord :: String -> Either ChordParserError Chord
parseChord s =
  case words s of
    [chordName, chordNotes] -> Right Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = Nothing
                  , baseFret = FZero
                     }
    [chordName, chordNotes, baseFret] -> Right Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = Nothing
                  , baseFret = read baseFret
                  }

    [chordName, chordNotes, baseFret, chordFingerings] -> Right Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = parseChordFingerings chordFingerings
                  , baseFret = read baseFret
                  }

    _ -> Left "wrong number of args"

parseSongChords :: String -> [Either ChordParserError Chord]
parseSongChords l = parseChord <$> lines l
