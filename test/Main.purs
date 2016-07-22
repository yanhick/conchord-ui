module Test.Main where

import Prelude ((==), ($), (<>), show, const, bind)

import Data.Foreign.Class (read, readJSON)
import Data.Foreign.Generic (toJSONGeneric, defaultOptions)
import Data.Foreign (toForeign)
import Data.Either (either)

import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser.String (eof)

import Test.StrongCheck (quickCheck, (<?>))


import Model (Song, SongMeta, SongLyric, parseSongMeta, Song, parseSong, serializeSong, serializeSongMeta, serializeSongLyric, parseSongLyric)
import Parser (SongChord)
import App (State)

main = do
    {--quickCheck \(c :: SongChord) ->--}
        {--let--}
            {--res = either (const false) ((==) c) (test c)--}
            {--test c' = read $ toForeign $ show c'--}
        {--in--}
            {--res <?> "SongChord encode/decode not idempotent for: " <> show c--}

    {--quickCheck \(s :: Song) ->--}
        {--let--}
            {--res = either (const false) ((==) s) (test s)--}
            {--test s' = readJSON $ toJSONGeneric defaultOptions s--}
        {--in--}
            {--res <?> "Song encode/decode not idempotent for: " <> show s--}

    {--quickCheck \(s :: State) ->--}
        {--let--}
            {--res = either (const false) ((==) s) (test s)--}
            {--test s' = readJSON $ toJSONGeneric defaultOptions s--}
        {--in--}
            {--res <?> "App State encode/decode not idempotent for: " <> show s--}

    {--quickCheck \(s :: SongMeta) ->--}
        {--let--}
            {--res = either (const false) ((==) s) (test s)--}
            {--test s' = runParser parseSongMeta (serializeSongMeta s)--}
        {--in--}
            {--res <?> "SongMeta parsing not idempotent for: " <> show s <> "\n" <> serializeSongMeta s--}

    quickCheck \(s :: SongLyric) ->
        let
            res = either (const false) ((==) s) (test s)
            test s' = runParser (parseSongLyric eof) (serializeSongLyric s)
        in
            res <?> "SongLyric parsing not idempotent for:\n\n" <> show s <> "\n\n" <>
            serializeSongLyric s <> "\n\nParsed:\n\n" <>
            show (runParser (parseSongLyric eof) (serializeSongLyric s))

    {--quickCheck \(s :: Song) ->--}
        {--let--}
            {--res = either (const false) ((==) s) (test s)--}
            {--test s' = runParser parseSong (serializeSong s)--}
        {--in--}
            {--res <?> "Song parsing not idempotent for: " <> show s <> "\n" <> serializeSong s--}






