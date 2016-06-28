module Parser where

import Prelude (pure, class Show, bind, ($), (>>=), Unit(), unit, (<>), show)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either())
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.String (take, drop)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Data.Identity (Identity, runIdentity)

newtype SongChord = SongChord {
    root :: SongChordRoot,
    rootModifier :: Maybe SongChordRootModifier
}

instance showSongChord :: Show SongChord where
    show (SongChord { root, rootModifier }) = show root <> show rootModifier

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

runSongChord (SongChord c) = c

chordRoot :: ChordParser
chordRoot = do
    (sc :: ChordParserState) <- get
    case toChordRoot (take 1 (fst sc)) of
        (Just r) -> do
            put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc) { root = r }))
            pure unit
        Nothing -> throwError ["Not A Valid Chord root"]

emptyChord :: SongChord
emptyChord = SongChord { root: A, rootModifier: Nothing }

chordRootModifier :: ChordParser
chordRootModifier = do
    (sc :: ChordParserState) <- get
    case toChordRootModifier (take 1 (fst sc)) of
      (Just r) -> do
          put $ Tuple (drop 1 (fst sc)) (SongChord ((runSongChord $ snd sc) { rootModifier = Just r }))
          pure unit
      Nothing -> pure unit

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
