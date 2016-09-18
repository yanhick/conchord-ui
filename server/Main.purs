module Main where

import Prelude ((<>), ($), bind, pure, Unit, id, show, (<<<))
import Data.Maybe (maybe, Maybe(..), isNothing)
import Data.Int (fromString)
import Data.Function.Uncurried (Fn3)
import Data.Tuple (Tuple(Tuple))
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Data.Foreign.Generic (defaultOptions, toJSONGeneric)
import Data.Either (either, Either(Left, Right))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error(), message, error, EXCEPTION(), catchException)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Database.Postgres (ConnectionInfo, DB)
import Node.Express.App (App(), listenHttp, get, post, put, delete, useOnError, useExternal)
import Node.Express.Types (EXPRESS, ExpressM, Request, Response)
import Node.Express.Handler (Handler(), nextThrow, HandlerM)
import Node.Express.Request (getRouteParam, getQueryParam, getBodyParam)
import Node.Express.Response (send, sendJson, sendFile, setStatus, redirect)
import Node.HTTP (Server())
import Network.HTTP.Affjax (AJAX())
import DOM (DOM())
import Signal.Channel (CHANNEL())

import Pux (renderToString, start)
import Signal ((~>))
import Route (Route(SearchResultPage, SongPage, NewSongPage, UpdateSongPage))
import App (init, AsyncData(LoadError, Loaded, Empty), State(State), UIState(UIState), IOState(IOState), SongUIState(SongUIState))
import Action (update)
import View (view)
import Text.Parsing.StringParser (runParser)

import Model (parseSong, serializeSong, exampleSong, DBSong(DBSong))
import DB (mkConnection, localConnectionInfo, getSongById, getSearchResults, createSong, updateSong, deleteSong)

foreign import urlencodedBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

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

appSetup :: forall e. ConnectionInfo -> App (console :: CONSOLE, db :: DB, err :: EXCEPTION | e)
appSetup c = do
    liftEff $ log "Setting up"
    useExternal urlencodedBodyParser
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
    put "/api/song/:id" (putUpdateSongApiHandler c)
    delete "/api/song/:id" (deleteSongPageHandler c)
    post "/api/song"   (postNewSongApiHandler c)
    useOnError         errorHandler


fileHandler :: forall e. Handler e
fileHandler = do
    fileName <- getRouteParam "file"
    sendFile $ maybe "index.html" id fileName

getUpdateSongPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE, err :: EXCEPTION | e ) Unit
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
                    ui: UIState { searchQuery: "", songUIState: SongUIState { showMenus: true, showSongMeta: true, showDuplicatedChorus: true, showSongSectionName: true } }
                })
            Nothing -> nextThrow $ error "Id is not a valid integer"

deleteSongPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE, err :: EXCEPTION | e ) Unit
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


putUpdateSongPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE, err :: EXCEPTION | e ) Unit
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
                      Right s' -> do
                          song' <- liftAff $ updateSong c id' s'
                          case song' of
                            Nothing -> do
                                setStatus 400
                                send "No song"
                            Just song'' -> case song'' of
                                            Left e' -> do
                                                setStatus 400
                                                send e'
                                            Right (DBSong { id: id''' }) -> do
                                                redirect $ "/song/" <> id'''

                  Nothing -> do
                      setStatus 400
                      send "No Song was sent"
            Nothing -> nextThrow $ error "Id is not a valid integer"

putUpdateSongApiHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE, err :: EXCEPTION | e ) Unit
putUpdateSongApiHandler c = do
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
                      Right s' -> do
                          song' <- liftAff $ updateSong c id' s'
                          case song' of
                            Nothing -> do
                                setStatus 400
                                send "No song"
                            Just song'' -> case song'' of
                                            Left e' -> do
                                                setStatus 400
                                                send e'
                                            Right song''' -> send $ toJSONGeneric defaultOptions song'''
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
        ui: UIState { searchQuery: "", songUIState: SongUIState { showMenus: true, showSongMeta: true, showDuplicatedChorus: true, showSongSectionName: true } }
    })

postNewSongApiHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
postNewSongApiHandler c = do
    song <- getBodyParam "song"
    case song of
      Just s ->
        case runParser parseSong s of
          Left e -> do
              setStatus 400
              send e
          Right s' -> do
              song' <- liftAff $ createSong c s'
              case song' of
                Nothing -> do
                    setStatus 400
                    send "No song"
                Just song'' -> case song'' of
                                Left e' -> do
                                    setStatus 400
                                    send e'
                                Right song''' -> send $ toJSONGeneric defaultOptions song'''

      Nothing -> do
          setStatus 400
          send "No Song was sent"

postNewSongPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
postNewSongPageHandler c = do
    song <- getBodyParam "song"
    case song of
      Just s ->
        case runParser parseSong s of
          Left e -> do
              setStatus 400
              send e
          Right s' -> do
              song' <- liftAff $ createSong c s'
              case song' of
                Nothing -> do
                    setStatus 400
                    send "No song"
                Just song'' -> case song'' of
                                Left e' -> do
                                    setStatus 400
                                    send e'
                                Right (DBSong { id }) -> do
                                    redirect $ "/song/" <> id

      Nothing -> do
          setStatus 400
          send "No Song was sent"

searchPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
searchPageHandler c = do
    q <- getQueryParam "q"
    case q of
      Just q' -> do
          result <- liftAff $ getSearchResults c q'
          case result of
            Left e -> do
                setStatus 400
                send e
            Right sr ->
                  send $ index (State {
                    currentPage: (SearchResultPage $ maybe "" id q),
                     io: IOState {
                         searchResults: Loaded (sr),
                         song: Empty,
                         newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
                         updateSong: Tuple (serializeSong exampleSong) (Right exampleSong)
                     },
                     ui: UIState { searchQuery: maybe "" id q, songUIState: SongUIState { showMenus: true, showSongMeta: true, showDuplicatedChorus: true, showSongSectionName: true } }
                  })
      Nothing -> nextThrow $ error "missing query param"

songPageHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songPageHandler c = do
    idParam <- getRouteParam "id"
    hideSongMeta <- getQueryParam "hide-song-meta"
    hideDuplicatedChords <- getQueryParam "hide-duplicated-chords"
    hideSongSectionName <- getQueryParam "hide-song-section-name"
    hideMenus <- getQueryParam "hide-menus"
    case idParam of
      Nothing -> nextThrow $ error "Id is required"
      Just id ->
        case fromString id of
          Just validId -> do
            s <- liftAff $ getSongById c validId
            send $ index (State {
                currentPage: (SongPage validId),
                io: IOState {
                    searchResults: Empty,
                    song: either (LoadError <<< show) Loaded $ runParser parseSong s,
                    newSong: Tuple (serializeSong exampleSong) (Right exampleSong),
                    updateSong: Tuple (serializeSong exampleSong) (Right exampleSong)
                },
                ui: UIState { searchQuery: "", songUIState: SongUIState { showMenus: isNothing hideMenus, showSongMeta: isNothing hideSongMeta, showDuplicatedChorus: isNothing hideDuplicatedChords, showSongSectionName: isNothing hideSongSectionName } }
            })
          Nothing -> nextThrow $ error "Id is not a valid integer"

homePageHandler :: forall e. Handler e
homePageHandler = send $ index init

searchApiHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
searchApiHandler c = do
    q <- getQueryParam "q"
    case q of
      Just q' -> do
          result <- liftAff $ getSearchResults c q'
          case result of
            Left e -> do
                setStatus 400
                send $ show e
            Right sr -> send $ toJSONGeneric defaultOptions $ sr
      Nothing -> nextThrow $ error "missing query param"


songApiHandler :: forall e. ConnectionInfo -> HandlerM ( express :: EXPRESS, db :: DB, console :: CONSOLE | e ) Unit
songApiHandler c = do
    idParam <- getRouteParam "id"
    case idParam of
      Nothing -> err "Id is required"
      Just id -> do
        case fromString id of
          Just validId -> do
            s <- liftAff $ getSongById c validId
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
            <script>document.body.className = 'js';</script>
            <script>window.puxLastState =  JSON.stringify(""" <> (toJSONGeneric defaultOptions s) <> """);</script>
            <script src="/app.js"></script>
        </body>
    </html>
    """
