module Parser where

import Prelude (pure, class Show, bind, ($), (>>=), Unit(), unit, (<>), show)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Either (Either(Left, Right))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.String (take, drop)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign (readString, F, ForeignError(TypeMismatch))
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Data.Identity (Identity, runIdentity)

instance isForeignSongChord :: IsForeign SongChord where
    read value = do
        s <- readString value
        case parseSongChord s of
          Left _ -> Left $ TypeMismatch s "Valid chord"
          Right (Tuple _ (Tuple _ c)) -> Right c

type SongChordFields = {
    root :: SongChordRoot,
    rootModifier :: Maybe SongChordRootModifier,
    quality :: SongChordQuality,
    interval :: Maybe SongChordInterval
}

newtype SongChord = SongChord SongChordFields

instance showSongChord :: Show SongChord where
    show (SongChord { root, rootModifier, quality, interval }) =
        show root <> (maybe "" show rootModifier) <> show quality <> (maybe "" show interval)

instance showChordQuality :: Show SongChordQuality where
    show Minor = "m"
    show Major = ""

instance showSongChordInterval :: Show SongChordInterval where
    show Seventh = "7"

data SongChordRoot = A | B | C | D | E | F | G

data SongChordRootModifier = Sharp | Flat

data SongChordQuality = Major | Minor

data SongChordInterval = Seventh

type Errors = Array String
type Log = Array String
type ChordParser = StateT ChordParserState (ExceptT Errors Identity) Unit

type ChordParserState = Tuple String SongChord

runChordParser :: ChordParser -> ChordParserState -> Either Errors (Tuple Unit ChordParserState)
runChordParser p s = runIdentity $ runExceptT $ runStateT p s

runSongChord :: forall a. SongChord -> SongChordFields
runSongChord (SongChord c) = c

parseSongChord :: String -> Either Errors (Tuple Unit ChordParserState)
parseSongChord s = runChordParser parse (Tuple s emptyChord)
    where
    parse = do
        chordRoot
        chordRootModifier
        chordQuality
        chordInterval

chordRoot :: ChordParser
chordRoot = do
    (sc :: ChordParserState) <- get
    case toChordRoot (take 1 (fst sc)) of
        (Just r) -> do
            put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc) { root = r }))
            pure unit
        Nothing -> throwError ["Not A Valid Chord root"]

emptyChord :: SongChord
emptyChord = SongChord { root: A, rootModifier: Nothing, quality: Major, interval: Nothing }

chordRootModifier :: ChordParser
chordRootModifier = do
    (sc :: ChordParserState) <- get
    case toChordRootModifier (take 1 (fst sc)) of
      (Just r) -> do
          put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc) { rootModifier = Just r }))
          pure unit
      Nothing -> pure unit

chordQuality :: ChordParser
chordQuality = do
    (sc :: ChordParserState) <- get
    case (take 1 (fst sc)) of
      "m" -> do
          put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc)) { quality = Minor })
          pure unit
      _ -> pure unit

chordInterval :: ChordParser
chordInterval = do
    (sc :: ChordParserState) <- get
    case (take 1 (fst sc)) of
      "7" -> do
          put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc)) { interval = Just Seventh })
          pure unit
      _ -> pure unit

toChordRootModifier :: String -> Maybe SongChordRootModifier
toChordRootModifier "#" = Just Sharp
toChordRootModifier "b" = Just Flat
toChordRootModifier _ = Nothing


toChordRoot :: String -> Maybe SongChordRoot
toChordRoot "A" = Just A
toChordRoot "B" = Just B
toChordRoot "C" = Just C
toChordRoot "D" = Just D
toChordRoot "E" = Just E
toChordRoot "F" = Just F
toChordRoot "G" = Just G
toChordRoot _ = Nothing

instance showSongChordRoot :: Show SongChordRoot where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"
    show G = "G"

instance showSongChordRootModifier :: Show SongChordRootModifier where
    show Sharp = "#"
    show Flat = "b"
