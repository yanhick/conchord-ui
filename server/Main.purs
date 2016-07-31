module Main where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.String (joinWith)
import Data.Int (fromString)
import Data.Function.Uncurried (Fn3)
import Data.Tuple (Tuple(Tuple))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Generic (defaultOptions, toJSONGeneric, readGeneric)
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
import Database.Postgres (ConnectionInfo, connect, DB, queryOne, query, query_, queryValue, Query(Query), execute, mkConnectionString)
import Database.Postgres.SqlValue (toSql)
import Node.Express.App (App(), listenHttp, get, post, put, delete, useOnError, useExternal)
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
import Route (Route(SearchResultPage, SongPage, NewSongPage, UpdateSongPage))
import App (init, AsyncData(LoadError, Loaded, Empty), State(State), UIState(UIState), IOState(IOState))
import Action (update)
import View (view)
import Text.Parsing.StringParser (runParser, ParseError(ParseError))

import Model (SearchResult(SearchResult), exampleSongMeta, parseSong, serializeSong, Song(Song), SongMeta(SongMeta), Year(Year), exampleSong)
import DB (mkConnection, localConnectionInfo, getSongById, getSearchResults, createSong, updateSong, deleteSong, SongTableRow)

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
    databaseUrl <- unsafeForeignFunction [""] "process.env.DATABASE_URL || ''"
    let connectionInfo = case runParser mkConnection databaseUrl of
              Right c -> c
              Left _ -> localConnectionInfo
    listenHttp (appSetup connectionInfo) port \_ ->
        log $ "listening on " <> show port

appSetup :: forall e. ConnectionInfo -> App (console :: CONSOLE, db :: DB | e)
appSetup c = do
    liftEff $ log "Setting up"
    useExternal jsonBodyParser
    get "/search"   (searchPageHandler c)
    get "/api/search"   (searchApiHandler c)
    get "/new"         getNewSongPageHandler
    post "/new"         (postNewSongPageHandler c)
    get "/update/:id"      (getUpdateSongPageHandler c)
    post "/update/:id"      (putUpdateSongPageHandler c)
    delete "/api/song/:id" (deleteSongPageHandler c)
    get "/song/:id"    (songPageHandler c)
    get "/:file"       fileHandler
    get "/"            homePageHandler
    get "/api/song/:id" (songApiHandler c)
    put "/api/song/:id" (putUpdateSongPageHandler c)
    delete "/api/song/:id" (deleteSongPageHandler c)
    post "/api/song"   (postNewSongPageHandler c)
    useOnError         errorHandler


fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

getUpdateSongPageHandler :: _
getUpdateSongPageHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
            Just id' -> do
                s <- liftAff $ getSongById c id'
                let s' = case runParser parseSong s of
                            Left e -> Tuple "" (Left $ show e)
                            Right song -> Tuple (serializeSong song) (Right song)
                send $ index (State {
                    currentPage: UpdateSongPage id',
                    io: IOState {
                        searchResults: Empty,
                        song: either (LoadError <<< show) Loaded $ runParser parseSong s,
                        newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
                        updateSong: s'
                    },
                    ui: UIState { searchQuery: "" }
                })
            Nothing -> nextThrow $ error "Id is not a valid integer"

deleteSongPageHandler :: _
deleteSongPageHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
          Just id' -> do
              liftEff $ launchAff $ deleteSong c id'
              setStatus 204
              send ""
          Nothing -> nextThrow $ error "Id is not a valid integer"


putUpdateSongPageHandler :: _
putUpdateSongPageHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
            Just id' -> do
                song <- getBodyParam "song"
                case song of
                  Just s ->
                    case runParser parseSong s of
                      Left e -> do
                          setStatus 400
                          send e
                      Right s -> do
                          liftEff $ launchAff $ updateSong c id' s
                          setStatus 204
                          send ""
                  Nothing -> do
                      setStatus 400
                      send "No Song was sent"
            Nothing -> nextThrow $ error "Id is not a valid integer"


getNewSongPageHandler :: forall e. Handler e
getNewSongPageHandler = do
    send $ index (State {
        currentPage: NewSongPage,
        io: IOState {
            searchResults: Empty,
            song: Empty,
            newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
            updateSong: Tuple (serializeSong exampleSong) (Right exampleSong)
        },
        ui: UIState { searchQuery: "" }
    })

postNewSongPageHandler :: _
postNewSongPageHandler c = do
    song <- getBodyParam "song"
    case song of
      Just s ->
        case runParser parseSong s of
          Left e -> do
              setStatus 400
              send e
          Right s -> do
              liftEff $ launchAff $ createSong c s
              setStatus 204
              send ""

      Nothing -> do
          setStatus 400
          send "No Song was sent"

searchPageHandler :: _
searchPageHandler c = do
    q <- getQueryParam "q"
    case q of
      Just q' -> do
          result <- liftAff $ getSearchResults c q'
          send $ index (State {
            currentPage: (SearchResultPage $ maybe "" id q),
             io: IOState {
                 searchResults: Loaded (result),
                 song: Empty,
                 newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
                 updateSong: Tuple (serializeSong exampleSong) (Right exampleSong)
             },
             ui: UIState { searchQuery: maybe "" id q }
          })
      Nothing -> nextThrow $ error "missing query param"

songPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songPageHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
          Just id -> do
            s <- liftAff $ getSongById c id
            send $ index (State {
                currentPage: (SongPage id),
                io: IOState {
                    searchResults: Empty,
                    song: either (LoadError <<< show) Loaded $ runParser parseSong s,
                    newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
                    updateSong: Tuple (serializeSong exampleSong) (Right exampleSong)
                },
                ui: UIState { searchQuery: "" }
            })
          Nothing -> nextThrow $ error "Id is not a valid integer"


homePageHandler :: forall e. Handler e
homePageHandler = send $ index init

searchApiHandler :: _
searchApiHandler c = do
    q <- getQueryParam "q"
    case q of
      Just q' -> do
          result <- liftAff $ getSearchResults c q'
          send $ toJSONGeneric defaultOptions $ result
      Nothing -> nextThrow $ error "missing query param"


songApiHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songApiHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> err "Id is required"
      Just id -> do
        case fromString id of
          Just id -> do
            s <- liftAff $ getSongById c id
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
