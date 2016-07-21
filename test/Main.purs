module Test.Main where

import Prelude ((==), ($), (<>), show, const, bind)

import Data.Foreign.Class (read, readJSON)
import Data.Foreign.Generic (toJSONGeneric, defaultOptions)
import Data.Foreign (toForeign)
import Data.Either (either)

import Test.StrongCheck (quickCheck, (<?>))


import Model (Song)
import Parser (SongChord)

main = do
    quickCheck \(c :: SongChord) ->
        let
            res = either (const false) ((==) c) (test c)
            test c' = read $ toForeign $ show c'
        in
            res <?> "SongChord encode/decode not idempotent for: " <> show c

    quickCheck \(s :: Song) ->
        let
            res = either (const false) ((==) s) (test s)
            test s' = readJSON $ toJSONGeneric defaultOptions s
        in
            res <?> "Song encode/decode not idempotent for: " <> show s
