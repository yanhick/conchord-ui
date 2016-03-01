module Main where

import Prelude
import Halogen
import Control.Monad.Eff
import Control.Monad.Eff.Console

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
