module Main where

import Prelude
import Data.Maybe (maybe)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Node.Express.App (App(), listenHttp, get)
import Node.Express.Types (EXPRESS)
import Node.Express.Handler (Handler())
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send, sendJson, sendFile, setResponseHeader)
import Node.HTTP (Server())

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

type Detail = { id :: Int, title :: String, desc :: String }

results :: String
results = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}]"

getDetails :: Int -> Detail
getDetails id = { id: 0, title: "", desc: "detail for:" <> show id }

details :: String
details = "{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}"

dummyListJson :: String
dummyListJson = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first element\"}]"

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/results" resultsHandler
    get "/details" detailsHandler
    get "/:file"   fileHandler
    get "/"   fileHandler

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

resultsHandler :: forall e. Handler e
resultsHandler = do
    setResponseHeader "Access-Control-Allow-Origin" "*"
    send results

detailsHandler :: forall e. Handler e
detailsHandler = do
    setResponseHeader "Access-Control-Allow-Origin" "*"
    idParam <- getRouteParam "id"
    sendJson getDetails

