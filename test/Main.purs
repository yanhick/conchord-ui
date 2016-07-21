module Test.Main where

import Prelude ((==), ($), (<>), show, const)

import Data.Foreign.Class (read)
import Data.Foreign (toForeign)
import Data.Either (either)

import Test.StrongCheck (quickCheck, (<?>))


import Parser (SongChord)

main = do
    quickCheck \(c :: SongChord) ->
        let
            res = either (const false) ((==) c) (test c)
            test c' = read $ toForeign $ show c'
        in
            res <?> "SongChord encode/decode not idempotent for: " <> show c
