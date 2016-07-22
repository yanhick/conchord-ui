module Main where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Generic (defaultOptions, toJSONGeneric)
import Data.Unfoldable (replicate)
import Data.Either (either)
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
import Route (Route(SearchResultPage, SongPage, NewSongPage))
import App (init, AsyncData(LoadError, Loaded, Empty), State(State), UIState(UIState), IOState(IOState), HeaderVisibility(ShowHeader))
import Action (update)
import View (view)
import Text.Parsing.StringParser (runParser)


import Model (SearchResult(SearchResult), exampleSong, exampleSongMeta, parseSong)

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

getSearchResult :: SearchResult
getSearchResult = SearchResult {
    id: 0,
    meta: exampleSongMeta,
    desc: "We're self-imploding, under the weight of your advice. I wear a suitcase, under each one of my eyes."
}

getSearchResults :: Array SearchResult
getSearchResults = replicate 15 getSearchResult

appSetup :: forall e. App (console :: CONSOLE | e)
appSetup = do
    liftEff $ log "Setting up"
    get "/new"         newSongPageHandler
    get "/song/:id"    songPageHandler
    get "/:file"       fileHandler
    get "/"            homePageHandler
    get "/api/search"   searchApiHandler
    get "/api/song/:id" songApiHandler
    useOnError         errorHandler

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

newSongPageHandler :: forall e. Handler e
newSongPageHandler = do
    send $ index (State {
        currentPage: NewSongPage,
        io: IOState { searchResults: Empty, song: Empty },
        ui: UIState { searchQuery: "", headerVisibility: ShowHeader }
    })

searchPageHandler :: forall e. Handler e
searchPageHandler = do
    qParam <- getQueryParam "q"
    send $ index (State {
        currentPage: (SearchResultPage $ maybe "" id qParam),
        io: IOState { searchResults: Loaded getSearchResults, song: Empty },
        ui: UIState { searchQuery: maybe "" id qParam, headerVisibility: ShowHeader }
    })

songPageHandler :: forall e. Handler e
songPageHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id -> do
        let s = unsafePerformEff $ catchException (\_ -> pure "") (readTextFile UTF8 "example-song.con")
        send $ index (State {
            currentPage: (SongPage 0),
            io: IOState { searchResults: Empty, song: either (LoadError <<< show) Loaded $ runParser parseSong s },
            ui: UIState { searchQuery: "", headerVisibility: ShowHeader }
        })

homePageHandler :: forall e. Handler e
homePageHandler = send $ index init

searchApiHandler :: forall e. Handler e
searchApiHandler = do
    qParam <- getQueryParam "q"
    send $ toJSONGeneric defaultOptions getSearchResults

songApiHandler :: forall e. Handler e
songApiHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id -> do
        send $ toJSONGeneric defaultOptions exampleSong

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
    err :: EXCEPTION,
    console :: CONSOLE
) String
renderApp s = do

    app <- start {
      initialState: s
    , update: update
    , view: view
    , inputs: []
    }

    renderToString app.html


index :: State -> String
index s =
    """
    <!doctype html>
    <html>
        <head>
            <link rel="stylesheet" href="/style.css">
        </head>
        <body>
            <div id="app">""" <> renderAppHandler s <> """</div>
                     <script>window.puxLastState =  JSON.stringify(""" <> (toJSONGeneric defaultOptions s) <> """);</script>
            <script src="/app.js"></script>
        </body>
    </html>
    """
