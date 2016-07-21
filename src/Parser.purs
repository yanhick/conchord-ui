module Parser where

import Prelude (pure, class Show, bind, ($), (>>=), Unit(), (<>), show, (<$>), (<*>), class Eq)
import Data.Functor (($>))
import Data.Maybe (Maybe(Just), maybe)
import Data.Either (Either(Left, Right))
import Data.Foreign.Class (class IsForeign)
import Data.Foreign (readString, ForeignError(TypeMismatch))
import Data.Generic (class Generic, gEq)
import Control.Alt ((<|>))

import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (string, eof)
import Text.Parsing.StringParser.Combinators (optionMaybe)

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Generic (gArbitrary)

instance isForeignSongChord :: IsForeign SongChord where
    read value = do
        s <- readString value
        case runParser parseChord s of
          Left _ -> Left $ TypeMismatch s "Valid chord"
          Right c -> Right c

newtype SongChord = SongChord {
    root :: SongChordRoot,
    rootModifier :: Maybe SongChordRootModifier,
    quality :: SongChordQuality,
    interval :: Maybe SongChordInterval
}

exampleChord :: SongChord
exampleChord = SongChord {
    root: A,
    rootModifier: Just Sharp,
    quality: Minor,
    interval: Just Seventh
}

derive instance genericSongChord :: Generic SongChord

instance arbitrarySongChord :: Arbitrary SongChord where
    arbitrary = gArbitrary

instance eqSongChord :: Eq SongChord where
    eq = gEq

instance showSongChord :: Show SongChord where
    show (SongChord { root, rootModifier, quality, interval }) =
        show root <> (maybe "" show rootModifier) <> show quality <> (maybe "" show interval)

instance showChordQuality :: Show SongChordQuality where
    show Minor = "m"
    show Major = ""

instance showSongChordInterval :: Show SongChordInterval where
    show Seventh = "7"

data SongChordRoot = A | B | C | D | E | F | G

derive instance genericSongChordRoot :: Generic SongChordRoot

data SongChordRootModifier = Sharp | Flat

derive instance genericSongChordRootModifier :: Generic SongChordRootModifier

data SongChordQuality = Major | Minor

derive instance genericSongChordQuality :: Generic SongChordQuality

data SongChordInterval = Seventh

derive instance genericSongChordInterval :: Generic SongChordInterval

parseRoot :: Parser SongChordRoot
parseRoot =  string "A" $> A
    <|> string "B" $> B
    <|> string "C" $> C
    <|> string "D" $> D
    <|> string "E" $> E
    <|> string "F" $> F
    <|> string "G" $> G

parseInterval :: Parser SongChordInterval
parseInterval = string "7" $> Seventh

parseQuality :: Parser SongChordQuality
parseQuality =  string "m" $> Minor
       <|> pure Major

parseModifier :: Parser SongChordRootModifier
parseModifier =  string "#" $> Sharp
        <|> string "b" $> Flat


parseChord :: Parser SongChord
parseChord = do
    r <- parseRoot
    (mkChord r) <$> optionMaybe parseModifier <*> parseQuality <*> optionMaybe parseInterval <*> eof

mkChord :: SongChordRoot -> Maybe SongChordRootModifier -> SongChordQuality -> Maybe SongChordInterval -> Unit -> SongChord
mkChord r m q i _ = SongChord { root: r, rootModifier: m, quality: q, interval: i }

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
