module SongChordsParser where

import Control.Applicative
import qualified Data.Text as T

{- refine (major, minor...) -}
data ChordName = ChordName T.Text deriving Show

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

parseChordName :: T.Text -> ChordName
parseChordName = ChordName

parseChordNotes :: T.Text -> Either ChordParserError ChordNotes
parseChordNotes ns =
  case T.unpack ns of
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

parseChordFingerings:: T.Text -> Maybe ChordFingerings
parseChordFingerings fs =
  case T.unpack fs of
    [lE, a, d, g, b, hE] -> Just $ GuitarStrings (parseFingering lE)
                                                (parseFingering a)
                                                (parseFingering d)
                                                (parseFingering g)
                                                (parseFingering b)
                                                (parseFingering hE)
    _ -> Nothing
  where parseFingering '-' = FOpened
        parseFingering t = FPressed $ read [t]

type ChordParserError = String

parseChord :: T.Text -> Either ChordParserError Chord
parseChord s =
  case T.words s of
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
                  , baseFret = read (T.unpack baseFret)
                  }

    [chordName, chordNotes, baseFret, chordFingerings] -> Right Chord {
                    name = parseChordName chordName
                  , notes = parseChordNotes chordNotes
                  , fingerings = parseChordFingerings chordFingerings
                  , baseFret = read (T.unpack baseFret)
                  }

    _ -> Left "wrong number of args"

parseAllSongChords :: [T.Text] -> [Chord] -> Either ChordParserError [Chord]
parseAllSongChords [] cs = Right cs
parseAllSongChords (x:xs) cs =
  case parseChord x of
    Left e -> Left e
    Right c -> parseAllSongChords xs (c:cs)


parseSongChords :: T.Text -> Either ChordParserError [Chord]
parseSongChords l = reverse <$> parseAllSongChords (T.lines l) []
