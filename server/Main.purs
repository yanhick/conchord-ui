module Main where

import Prelude
import Data.Int (fromString)
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message, error)
import Control.Monad.Eff.Class (liftEff)
import Node.Express.App (App(), listenHttp, get, useOnError)
import Node.Express.Types (EXPRESS)
import Node.Express.Handler (Handler(), nextThrow)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send, sendJson, sendFile, setStatus)
import Node.HTTP (Server())

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

type Detail = { id :: Int, title :: String, desc :: String }
type Results = Array Detail

getResults :: Results
getResults = [{id: 0, title: "first", desc: "this is the first element"}]

getDetails :: Int -> Detail
getDetails id = { id: 0, title: "", desc: "detail for:" <> show id }

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/results"     resultsHandler
    get "/details/:id" detailsHandler
    get "/:file"       fileHandler
    get "/"            fileHandler
    useOnError         errorHandler

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

resultsHandler :: forall e. Handler e
resultsHandler = do
    send getResults

detailsHandler :: forall e. Handler e
detailsHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id -> do
        sendJson $ getDetails $ fromMaybe 0 (fromString id)

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
    setStatus 400
    sendJson {error: message err}

