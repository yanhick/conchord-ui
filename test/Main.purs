module Test.Main where

import Prelude ((==), ($), Unit, show, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Data.Either (Either (Left, Right))
import Test.QuickCheck (quickCheck, (<?>))

import Model (SongMeta())

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | e) Unit
main = do
    log "test"
