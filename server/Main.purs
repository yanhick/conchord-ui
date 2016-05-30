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
import Node.Express.Request (getRouteParam, getQueryParam)
import Node.Express.Response (send, sendJson, sendFile, setStatus)
import Node.HTTP (Server())

import Model (SearchResults, SearchResult(SearchResult))

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    listenHttp appSetup 8080 \_ ->
        log $ "listening on " <> show 8080

type Detail = { id :: Int, title :: String, desc :: String }

getSearchResults :: String -> SearchResults
getSearchResults q = [
        SearchResult {id: 0, title: q, desc: "this is the first result for: " <> q },
        SearchResult {id: 1, title: q, desc: "this is the second result for: " <> q }
    ]

getSong :: Int -> Detail
getSong id = { id: id, title: "", desc: "detail for:" <> show id }

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/search"     searchHandler
    get "/song/:id" songHandler
    get "/:file"       fileHandler
    get "/"            fileHandler
    useOnError         errorHandler

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

searchHandler :: forall e. Handler e
searchHandler = do
    qParam <- getQueryParam "q"
    send $ getSearchResults (fromMaybe "" qParam)

songHandler :: forall e. Handler e
songHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id -> do
        sendJson $ getSong $ fromMaybe 0 (fromString id)

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
    setStatus 400
    sendJson {error: message err}

