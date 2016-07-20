module ParserAlt where

import Prelude (pure, bind, ($), (<$>), (<*>), Unit)

import Control.Alt ((<|>))
import Data.Functor (($>))
import Data.Maybe (Maybe(Nothing))

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string, eof)
import Text.Parsing.StringParser.Combinators (optionMaybe)

import Parser (SongChord(SongChord), SongChordFields, SongChordRoot(..), SongChordRootModifier(..), SongChordQuality(..), SongChordInterval(..), emptyChord)

root :: Parser SongChordRoot
root =  string "A" $> A
    <|> string "B" $> B
    <|> string "C" $> C
    <|> string "D" $> D
    <|> string "E" $> E
    <|> string "F" $> F
    <|> string "G" $> G

interval :: Parser SongChordInterval
interval = string "7" $> Seventh

quality :: Parser SongChordQuality
quality =  string "m" $> Minor
       <|> pure Major

modifier :: Parser SongChordRootModifier
modifier =  string "#" $> Sharp
        <|> string "b" $> Flat


chord :: Parser SongChord
chord = do
    r <- root
    (getChord r) <$> optionMaybe modifier <*> quality <*> optionMaybe interval <*> eof

getChord :: SongChordRoot -> Maybe SongChordRootModifier -> SongChordQuality -> Maybe SongChordInterval -> Unit -> SongChord
getChord r m q i _ = SongChord { root: r, rootModifier: m, quality: q, interval: i }
