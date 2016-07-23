module Main where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.String (joinWith)
import Data.Function.Uncurried (Fn3)
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Generic (defaultOptions, toJSONGeneric)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Unfoldable (replicate)
import Data.Either (either, Either(Left, Right))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message, error, EXCEPTION(), catchException)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Database.Postgres (connect, DB, query_, Query(Query))
import Node.Express.App (App(), listenHttp, get, post, useOnError, useExternal)
import Node.Express.Types (EXPRESS, ExpressM, Request, Response)
import Node.Express.Handler (Handler(), nextThrow)
import Node.Express.Request (getRouteParam, getQueryParam, getBodyParam)
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

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

main :: Eff (
    console :: CONSOLE,
    express :: EXPRESS,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION,
    db :: DB
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
    useExternal jsonBodyParser
    get "/testdb"       testDb
    get "/new"         getNewSongPageHandler
    post "/new"         postNewSongPageHandler
    get "/song/:id"    songPageHandler
    get "/:file"       fileHandler
    get "/"            homePageHandler
    get "/api/search"   searchApiHandler
    get "/api/song/:id" songApiHandler
    post "/api/song"   postNewSongPageHandler
    useOnError         errorHandler

data Artist = Artist { name :: String, year :: Int }

instance artistIsForeign :: IsForeign Artist where
    read obj = do
        name <- readProp "name" obj
        year <- readProp "year" obj
        pure $ Artist { name, year }

instance artistShow :: Show Artist where
    show (Artist p) = "Artist (" <> p.name <> ", " <> show p.year <> ")"

connectionInfo = {
    host: "localhost",
    db: "test",
    port: 5432,
    user: "testuser",
    password: "test"
}


testDb :: _
testDb = do
    liftEff $ launchAff $ do
        client <- connect connectionInfo
        artists <- query_ (Query "select * from artist" :: Query Artist) client
        logShow $ joinWith "\n" (show <$> artists)
        logShow "bim"
    send "test"

fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

getNewSongPageHandler :: forall e. Handler e
getNewSongPageHandler = do
    send $ index (State {
        currentPage: NewSongPage,
        io: IOState { searchResults: Empty, song: Empty },
        ui: UIState { searchQuery: "", headerVisibility: ShowHeader, newSong: "" }
    })

postNewSongPageHandler :: forall e. Handler e
postNewSongPageHandler = do
    song <- getBodyParam "song"
    case song of
      Just s ->
        case runParser parseSong s of
          Left s -> do
              setStatus 400
              send s
          Right _ -> do
              setStatus 204
              send ""

      Nothing -> do
          setStatus 400
          send "No Song was sent"

searchPageHandler :: forall e. Handler e
searchPageHandler = do
    qParam <- getQueryParam "q"
    send $ index (State {
        currentPage: (SearchResultPage $ maybe "" id qParam),
        io: IOState { searchResults: Loaded getSearchResults, song: Empty },
        ui: UIState { searchQuery: maybe "" id qParam, headerVisibility: ShowHeader, newSong: "" }
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
            ui: UIState { searchQuery: "", headerVisibility: ShowHeader, newSong: "" }
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
