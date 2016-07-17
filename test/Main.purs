module Test.Main where

import Prelude ((==), ($), Unit, show, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut (encodeJson)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Either (Either (Left, Right))
import Test.QuickCheck (quickCheck, (<?>))

import Model (SongMeta())

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | e) Unit
main = do
    quickCheck \s -> result s <?> "SongMeta encode/decode not idempotent"
        where
        result s = case (readJSON $ show $ encodeJson s) :: F SongMeta of
            Left _ -> false
            Right s' -> s == s'
