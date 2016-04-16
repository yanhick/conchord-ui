module Main where

import Prelude
import Control.Monad.Eff.Console (print, CONSOLE)
import Control.Monad.Eff (Eff())

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = print "Hello"
