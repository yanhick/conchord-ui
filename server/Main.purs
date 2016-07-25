module Main where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.String (joinWith)
import Data.Int (fromString)
import Data.Function.Uncurried (Fn3)
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Generic (defaultOptions, toJSONGeneric)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Unfoldable (replicate)
import Data.Either (either, Either(Left, Right))
import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message, error, EXCEPTION(), throwException, catchException, error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Database.Postgres (connect, DB, queryOne, query, query_, queryValue, Query(Query), execute)
import Database.Postgres.SqlValue (toSql)
import Node.Express.App (App(), listenHttp, get, post, useOnError, useExternal)
import Node.Express.Types (EXPRESS, ExpressM, Request, Response)
import Node.Express.Handler (Handler(), nextThrow, HandlerM)
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
import Text.Parsing.StringParser (runParser, ParseError(ParseError))

import Model (SearchResult(SearchResult), exampleSong, exampleSongMeta, parseSong, serializeSong, Song(Song), SongMeta(SongMeta), Year(Year))

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

appSetup :: forall e. App (console :: CONSOLE, db :: DB | e)
appSetup = do
    liftEff $ log "Setting up"
    useExternal jsonBodyParser
    get "/new"         getNewSongPageHandler
    post "/new"         postNewSongPageHandler
    get "/song/:id"    songPageHandler
    get "/:file"       fileHandler
    get "/"            homePageHandler
    get "/api/search"   searchApiHandler
    get "/api/song/:id" songApiHandler
    post "/api/song"   postNewSongPageHandler
    useOnError         errorHandler

connectionInfo = {
    host: "localhost",
    db: "test",
    port: 5432,
    user: "testuser",
    password: "test"
}

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

postNewSongPageHandler :: _
postNewSongPageHandler = do
    song <- getBodyParam "song"
    case song of
      Just s ->
        case runParser parseSong s of
          Left e -> do
              setStatus 400
              send e
          Right song@(Song { meta: SongMeta m@{ year: Year y } })-> do
              liftEff $ launchAff $ do
                  client <- connect connectionInfo
                  execute (Query "insert into song values(default, $1, $2, $3, $4, $5)") [
                      toSql m.title,
                      toSql m.artist,
                      toSql m.album,
                      toSql y,
                      toSql $ serializeSong (song)
                  ] client
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

songPageHandler :: forall e. HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songPageHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
          Just id -> do
            s <- liftAff $ getSongById id
            send $ index (State {
                currentPage: (SongPage id),
                io: IOState { searchResults: Empty, song: either (LoadError <<< show) Loaded $ runParser parseSong s },
                ui: UIState { searchQuery: "", headerVisibility: ShowHeader, newSong: "" }
            })
          Nothing -> nextThrow $ error "Id is not a valid integer"

getSongById :: forall e. Int -> Aff ( db :: DB, console :: CONSOLE | e ) String
getSongById id = do
    client <- connect connectionInfo
    content <- queryValue (Query "select content from song where id = $1" :: Query String) [toSql id] client
    pure $ case content of
          Just s -> s
          Nothing -> ""

homePageHandler :: forall e. Handler e
homePageHandler = send $ index init

searchApiHandler :: forall e. Handler e
searchApiHandler = do
    qParam <- getQueryParam "q"
    send $ toJSONGeneric defaultOptions getSearchResults

songApiHandler :: forall e. HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songApiHandler = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> err "Id is required"
      Just id -> do
        case fromString id of
          Just id -> do
            s <- liftAff $ getSongById id
            let s' = runParser parseSong s
            case s' of
              Right s'' -> send $ toJSONGeneric defaultOptions s''
              Left e -> err (show e)
          Nothing -> err "Id is not a valid integer"
    where err = nextThrow <<< error

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
