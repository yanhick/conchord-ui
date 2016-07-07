module Main where

import Prelude
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Array (replicate)
import Data.String (joinWith)
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

import Model (SearchResults, SearchResult(SearchResult), SongMeta(SongMeta), Year(Year))

main :: forall eff. Eff (console :: CONSOLE, express :: EXPRESS | eff) Server
main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listenHttp appSetup port \_ ->
        log $ "listening on " <> show port

getSongMeta :: String
getSongMeta = "{ \"title\": \"Tokyo vampires and wolves\", \"artist\":\"The Wombats\", \"album\":\"This modern glitch\", \"year\": 2011 }"

getSearchResult :: String
getSearchResult = "{\"id\":0, \"meta\":" <> getSongMeta <> ", \"desc\":\"We're self-imploding, under the weight of your advice. I wear a suitcase, under each one of my eyes.\"}"

getSearchResults :: String
getSearchResults = "[" <> joinWith "," (replicate 15 (getSearchResult)) <> "]"

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
    send $ getSearchResults

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

