module Parser where

import Prelude (pure, class Show, bind, ($), (>>=))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Data.Identity (Identity, runIdentity)

type SongChord = {
    root :: SongChordRoot,
    quality :: SongChordQuality,
    interval :: Maybe SongChordInterval
}

data SongChordRoot = A | B | C | D | E | F | G

data SongChordRootModifier = None | Sharp | Flat

data SongChordQuality = Major | Minor

data SongChordInterval = Seventh

type Errors = Array String
type Log = Array String
type ChordParser = StateT ChordParserState (WriterT Log (ExceptT Errors Identity))

type ChordParserState = Tuple String SongChordRoot

runChordParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

chordRoot :: ChordParser ChordParserState
chordRoot = get >>=
    \(Tuple s c) ->
        case toChordRoot s of
          (Just r) -> do
              put (Tuple s r)
              pure (Tuple s c)
          Nothing -> throwError ["Not A Valid Chord root"]

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
