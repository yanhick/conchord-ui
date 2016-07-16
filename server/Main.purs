module Main where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Class (readJSON)
import Data.Unfoldable (replicate)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message, error, EXCEPTION(), catchException)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Node.Express.App (App(), listenHttp, get, useOnError)
import Node.Express.Types (EXPRESS)
import Node.Express.Handler (Handler(), nextThrow)
import Node.Express.Request (getRouteParam, getQueryParam)
import Node.Express.Response (send, sendJson, sendFile, setStatus)
import Node.HTTP (Server())
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Network.HTTP.Affjax (AJAX())
import DOM (DOM())
import Signal.Channel (CHANNEL())

import Pux (renderToString, start)
import Signal ((~>))
import Route (Route(SearchResultPage, SongPage))
import App (init, AsyncData(Loaded, Empty), State)
import Action (update)
import View (view)


import Model (SongMeta(SongMeta), Year(Year), SearchResult(SearchResult))
import Data.Argonaut (encodeJson)

main :: Eff (
    console :: CONSOLE,
    express :: EXPRESS,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION
) Server
main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listenHttp appSetup port \_ ->
        log $ "listening on " <> show port

getSongMeta :: SongMeta
getSongMeta = SongMeta {
    title: "Tokyo vampires and wolves",
    artist: "The Wombats",
    album: Just "This modern glitch",
    year: Year 2011
}

getSearchResult :: SearchResult
getSearchResult = SearchResult {
    id: 0,
    meta: getSongMeta,
    desc: "We're self-imploding, under the weight of your advice. I wear a suitcase, under each one of my eyes."
}

getSearchResults :: Array SearchResult
getSearchResults = replicate 15 getSearchResult

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/search"      searchPageHandler
    get "/song/:id"    songPageHandler
    get "/:file"       fileHandler
    get "/"            homePageHandler
    get "/api/search"   searchApiHandler
    get "/api/song/:id" songHandler
    useOnError         errorHandler

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

searchPageHandler :: forall e. Handler e
searchPageHandler = do
    qParam <- getQueryParam "q"
    send $ index (renderAppHandler init {
        currentPage = (SearchResultPage $ maybe "" id qParam),
        io = { searchResults: Loaded(pure getSearchResults), song: Empty }
    })

songPageHandler :: forall e. Handler e
songPageHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id -> do
        let s = unsafePerformEff $ catchException (\_ -> pure "") (readTextFile UTF8 "song.json")
        send $ index (renderAppHandler init {
            currentPage = (SongPage 0),
            io = { searchResults: Empty, song: Loaded(readJSON s)}
        })

homePageHandler :: forall e. Handler e
homePageHandler = send $ index (renderAppHandler init)

searchApiHandler :: forall e. Handler e
searchApiHandler = do
    qParam <- getQueryParam "q"
    sendJson $ encodeJson getSearchResults

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


renderAppHandler :: State -> String
renderAppHandler s = unsafePerformEff $ catchException (\_ -> pure "") (renderApp s)

renderApp :: State -> Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION
) String
renderApp s = do

    app <- start {
      initialState: s
    , update: update
    , view: view
    , inputs: []
    }

    renderToString app.html


index :: String -> String
index s =
    """
    <!doctype html>
    <html>
        <head>
            <link rel="stylesheet" href="/style.css">
        </head>
        <body>
            <div id="app">
     """
     <> s <>
     """
            <script src="/app.js"></script>
            </div>
        </body>
    </html>
    """
