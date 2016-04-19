module Main where

import Prelude
import Control.Monad.Eff.Console (print, log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Node.Express.App (App(), listenHttp, get)
import Node.Express.Types (EXPRESS)
import Node.Express.Handler (Handler())
import Node.Express.Response (send, setResponseHeader)
import Node.HTTP (Server())

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

results :: String
results = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}]"

details :: String
details = "{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}"

dummyListJson :: String
dummyListJson = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}]"

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/results" resultsHandler
    get "/details" detailsHandler

resultsHandler :: forall e. Handler e
resultsHandler = do
    setResponseHeader "Access-Control-Allow-Origin" "*"
    send results

detailsHandler :: forall e. Handler e
detailsHandler = do
    setResponseHeader "Access-Control-Allow-Origin" "*"
    send details

