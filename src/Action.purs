module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-), (==), (<<<), not)

import Data.Argonaut (class EncodeJson, (~>), (:=), jsonEmptyObject, encodeJson)
import Data.FormURLEncoded (fromArray, encode)
import Data.Maybe (Maybe(Just))
import Data.Either (Either (Left, Right), either)
import Data.Foreign (F)
import Data.Foreign.Generic (toJSONGeneric, defaultOptions)
import Data.Tuple (Tuple(Tuple))
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX(), get, post, put, delete, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Data.HTTP.Method (Method(POST, PUT))
import Data.MediaType.Common (applicationFormURLEncoded)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route(SongPage, SearchResultPage, UpdateSongPage, NewSongPage, HomePage))
import Model (SearchResults, SongsList, Song, serializeSong, parseSong, DBSong(DBSong), exampleSong)
import App (State(State), UIState(UIState), SongUIState(SongUIState), IOState(IOState), AsyncData(Loading, Loaded, LoadError))
import Text.Parsing.StringParser (runParser)


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route |
    Noop

data UIAction =
    SearchChange FormEvent |
    NewSongChange FormEvent |
    UpdateSongChange FormEvent |
    ToggleShowSongMeta |
    ToggleShowDuplicatedChorus |
    ToggleShowSongSectionName |
    ToggleShowMenus

data IOAction =
    RequestSongsList |
    ReceiveSongsList (F SongsList) |
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch String |
    ReceiveSearch (F SearchResults) |
    SubmitNewSong |
    SubmitUpdateSong |
    SubmitDeleteSong Int |
    ReceiveSubmitNewSong PostNewSongResponse |
    ReceiveSubmitUpdateSong Int PostResponse |
    ReceiveSubmitDeleteSong PostResponse

data PostResponse = Ok | Ko String
type PostNewSongResponse = Either String DBSong

type Affction = EffModel State Action (ajax :: AJAX, dom :: DOM, console :: CONSOLE)


update :: Action -> State -> Affction
update (PageView p) state = updatePage p state
update (IOAction a) state = updateIO a state
update (UIAction a) state = updateUI a state
update Noop state = noEffects state

--- PageView Actions

updatePage :: Route -> State -> Affction
updatePage r (State state@{ currentPage: r' })
    | r == r' = noEffects $ State state
updatePage p@(SongPage s) (State state) =
    updateIO (RequestSong s) (State (state { currentPage = p }))
updatePage p@(SearchResultPage q) (State state@{ ui: UIState ui }) =
    updateIO (RequestSearch q) (State state { currentPage = p, ui = UIState ui { searchQuery = q } })
updatePage p@(UpdateSongPage _) (State state@{ io: IOState { song: song@Loaded(song'), searchResults, songsList, newSong  } }) =
    noEffects $ State state { currentPage = p, io = IOState { updateSong: Tuple (serializeSong song') (Right song'), song, searchResults, songsList, newSong } }
updatePage p@(NewSongPage) (State state@{ io: IOState io }) =
    noEffects $ State state { currentPage = p, io = IOState io { newSong = Tuple (serializeSong exampleSong) (Right exampleSong) } }
updatePage p@(HomePage) (State state) =
    updateIO RequestSongsList (State state { currentPage = p })
updatePage p (State state) = noEffects $ State state { currentPage = p }


--- UI Actions

updateUI :: UIAction -> State -> Affction

updateUI (SearchChange { target: { value } }) (State state@{ ui: UIState ui }) =
    noEffects $ State state { ui = UIState ui { searchQuery = value } }

updateUI (NewSongChange { target: { value } }) (State state@{ io: IOState { searchResults, songsList, song, updateSong } }) =
    noEffects $ State state { io = IOState { song, searchResults, songsList, updateSong, newSong: Tuple value (either (Left <<< show) Right $ runParser parseSong value) } }

updateUI (UpdateSongChange { target: { value } }) (State state@{ io: IOState { newSong, song, searchResults, songsList } }) =
    noEffects $ State state { io = IOState { song, searchResults, songsList, newSong, updateSong: Tuple value (either (Left <<< show) Right $ runParser parseSong value) } }

updateUI ToggleShowSongMeta (State state@{ ui: UIState ui@{ songUIState: SongUIState songUI@{ showSongMeta } } }) =
    noEffects $ State state { ui = UIState ui { songUIState = SongUIState songUI { showSongMeta = not showSongMeta } } }

updateUI ToggleShowDuplicatedChorus (State state@{ ui: UIState ui@{ songUIState: SongUIState songUI@{ showDuplicatedChorus } } }) =
    noEffects $ State state { ui = UIState ui { songUIState = SongUIState songUI { showDuplicatedChorus = not showDuplicatedChorus } } }

updateUI ToggleShowSongSectionName (State state@{ ui: UIState ui@{ songUIState: SongUIState songUI@{ showSongSectionName } } }) =
    noEffects $ State state { ui = UIState ui { songUIState = SongUIState songUI { showSongSectionName = not showSongSectionName } } }

updateUI ToggleShowMenus (State state@{ ui: UIState ui@{ songUIState: SongUIState songUI@{ showMenus } } }) =
    noEffects $ State state { ui = UIState ui { songUIState = SongUIState songUI { showMenus = not showMenus } } }

--- IO Actions

updateIO :: IOAction -> State -> Affction

updateIO RequestSongsList (State state@{ io: IOState { searchResults, song, newSong, updateSong }}) = {
    state: State $ state { io = IOState { searchResults, song, newSong, updateSong, songsList: Loading } }
    , effects: [ do
        res <- fetchSongsList
        let results = (readJSON res) :: F SongsList
        pure $ IOAction $ ReceiveSongsList results
    ]
}

updateIO (ReceiveSongsList (Right r)) (State state@{ io: IOState { song, newSong, updateSong, searchResults } }) =
    noEffects $ State $ state { io = IOState { searchResults, song, newSong, updateSong, songsList: Loaded r } }

updateIO (ReceiveSongsList (Left e)) (State state@{ io: IOState { song, newSong, updateSong, searchResults } }) =
    noEffects $ State $ state { io = IOState { searchResults, song, newSong, updateSong, songsList: LoadError (show e) } }


updateIO (RequestSearch q) (State state@{ ui: UIState { searchQuery }, io: IOState { song, newSong, updateSong, songsList }}) = {
    state: State $ state { io = IOState { searchResults: Loading, song, newSong, updateSong, songsList } }
  , effects: [ do
        liftEff $ navigateTo $ "/search?q=" <> q
        res <- fetchSearch searchQuery
        let results = (readJSON res) :: F SearchResults
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIO (ReceiveSearch (Right r)) (State state@{ io: IOState { song, newSong, updateSong, songsList } }) =
    noEffects $ State $ state { io = IOState { searchResults: Loaded r, song, newSong, updateSong, songsList } }

updateIO (ReceiveSearch (Left e)) (State state@{ io: IOState { song, newSong, updateSong, songsList } }) =
    noEffects $ State $ state { io = IOState { searchResults: LoadError (show e), song, newSong, updateSong, songsList } }

updateIO (RequestSong id) (State state@{ io: IOState { searchResults, newSong, updateSong, songsList } }) = {
    state: State $ state { io = IOState { song: Loading, searchResults, newSong, updateSong, songsList } }
  , effects: [ do
        res <- fetchSong id
        let song = (readJSON res) :: F Song
        pure $ IOAction $ ReceiveSong song
    ]
}

updateIO (ReceiveSong (Right s)) (State state@{ io: IOState { searchResults, newSong, updateSong, songsList } }) =
    noEffects $ State state { io = IOState { song: Loaded s, searchResults, newSong, updateSong, songsList } }

updateIO (ReceiveSong (Left e)) (State state@{ io: IOState { searchResults, newSong, updateSong, songsList } }) =
    noEffects $ State state { io = IOState { song: LoadError (show e), searchResults, newSong, updateSong, songsList } }

updateIO SubmitNewSong state@(State { io: IOState { newSong: Tuple s (Right _) } }) = {
    state: state,
    effects: [ do
        res <- postSong (PostSong s)
        pure $ IOAction $ ReceiveSubmitNewSong res
    ]
}

updateIO SubmitNewSong state = noEffects state

updateIO SubmitUpdateSong state@(State { currentPage: (UpdateSongPage id) , io: IOState { updateSong: Tuple s (Right _) } }) = {
    state: state,
    effects: [ do
        res <- updateSong' id (PostSong s)
        pure $ IOAction $ ReceiveSubmitUpdateSong id res
    ]
}

updateIO SubmitUpdateSong state = noEffects state

updateIO (SubmitDeleteSong id) state = {
    state: state,
    effects: [ do
        res <- deleteSong id
        pure $ IOAction $ ReceiveSubmitDeleteSong res
    ]
}


updateIO (ReceiveSubmitNewSong (Right (DBSong { id, song }))) (State state@{ io: IOState io }) = {
    state: State state { io = IOState io { song = Loaded song } }
  , effects: [ do
        liftEff $ navigateTo $ "/song/" <> id
        pure Noop
    ]
}

updateIO (ReceiveSubmitNewSong (Left e)) (State state@{ io: IOState { searchResults, songsList, song, newSong: Tuple s _, updateSong } }) =
    noEffects $ State state { io = IOState { searchResults, song, newSong: Tuple s (Left e), updateSong, songsList } }

updateIO (ReceiveSubmitUpdateSong id Ok) state = {
    state: state,
    effects: [ do
        liftEff $ navigateTo $ "/song/" <> show id
        pure Noop
    ]
}
updateIO (ReceiveSubmitUpdateSong _ (Ko e)) (State state@{ io: IOState { searchResults, songsList, song, updateSong: Tuple s _, newSong } }) =
    noEffects $ State state { io = IOState { searchResults, songsList, song, updateSong: Tuple s (Left e), newSong } }

updateIO (ReceiveSubmitDeleteSong Ok) state = {
    state: state,
    effects: [ do
        liftEff $ navigateTo "/"
        pure Noop
    ]
}
updateIO (ReceiveSubmitDeleteSong (Ko e)) (State state@{ io: IOState { searchResults, songsList, song, newSong: Tuple s _, updateSong } }) =
    noEffects $ State state { io = IOState { searchResults, songsList, song, newSong: Tuple s (Left e), updateSong } }

--- AJAX Requests

fetchSearch :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchSearch q = do
    result <- get $ "/api/search?q=" <> q
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"

fetchSong :: forall eff. Int -> Aff (ajax :: AJAX | eff) String
fetchSong id = do
    result <- get $ "/api/song/" <> show id
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"

fetchSongsList :: forall eff. Aff (ajax :: AJAX | eff) String
fetchSongsList = do
    result <- get $ "/api/song"
    pure case result.status of
           (StatusCode 200) -> result.response
           _ -> "fail"

newtype PostSong = PostSong String

instance postSongEncodeJson :: EncodeJson PostSong where
    encodeJson (PostSong song)
        = "song" := song
        ~> jsonEmptyObject

postSong :: forall eff. PostSong -> Aff (ajax :: AJAX | eff) PostNewSongResponse
postSong s@(PostSong s') = do
    result <- affjax $ defaultRequest { method = Left POST, url = "/api/song", content = Just $ encode (fromArray [ Tuple "song" (Just s')]), headers = [ ContentType applicationFormURLEncoded ] }
    pure case result.status of
        (StatusCode 200) -> case readJSON result.response of
                              Right s -> Right s
                              Left e -> Left $ show e
        _ -> Left result.response

updateSong' :: forall eff. Int -> PostSong -> Aff (ajax :: AJAX | eff) PostResponse
updateSong' id (PostSong s') = do
    result <- affjax $ defaultRequest { method = Left PUT, url = ("/api/song/" <> show id), content = Just $ encode (fromArray [ Tuple "song" (Just s')]), headers = [ ContentType applicationFormURLEncoded ] }
    pure case result.status of
        (StatusCode 200) -> Ok
        _ -> Ko result.response

deleteSong :: forall eff. Int -> Aff (ajax :: AJAX | eff) PostResponse
deleteSong id = do
    result <- delete ("/api/song/" <> show id)
    pure case result.status of
        (StatusCode 204) -> Ok
        _ -> Ko result.response
