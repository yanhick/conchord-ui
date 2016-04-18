module Main where

import Prelude
import Control.Monad.Eff.Console (print, log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Node.Express.App (App(), listenHttp, get)
import Node.Express.Types (EXPRESS)
import Node.Express.Handler (Handler())
import Node.Express.Response (sendJson, setResponseHeader)
import Node.HTTP (Server())

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

dummyListJson :: String
dummyListJson = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}]"

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/results" resultsHandler

resultsHandler :: forall e. Handler e
resultsHandler = do
    setResponseHeader "Access-Control-Allow-Origin" "*"
    sendJson dummyListJson
