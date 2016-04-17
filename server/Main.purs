module Main where

import Prelude
import Control.Monad.Eff.Console (print, log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Node.Express.App (App(), listenHttp, get)
import Node.Express.Types (EXPRESS)
import Node.Express.Response (send)
import Node.HTTP (Server())

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/" (send "hello")

