module ParserAlt where

import Prelude (pure, bind, ($))

import Control.Alt ((<|>))
import Data.Functor (($>))
import Data.Maybe (Maybe(Nothing))

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.String (string)
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
quality =   string "m" $> Minor
        <|> pure Major


chord :: Parser SongChord
chord = do
    r <- root
    q <- quality
    i <- optionMaybe interval
    pure $ SongChord { root: r, rootModifier: Nothing, quality: q, interval: i }
