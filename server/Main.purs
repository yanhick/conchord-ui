module Main where

import Prelude
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
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
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listenHttp appSetup port \_ ->
        log $ "listening on " <> show port

getSearchResults :: String -> SearchResults
getSearchResults q = [
        SearchResult {id: 0, title: q, desc: "this is the first result for: " <> q },
        SearchResult {id: 1, title: q, desc: "this is the second result for: " <> q }
    ]

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
        sendFile "song.json"

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
    setStatus 400
    sendJson {error: message err}

