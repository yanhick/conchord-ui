module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-), (==), (<<<))

import Data.Argonaut (class EncodeJson, (~>), (:=), jsonEmptyObject, encodeJson)
import Data.Either (Either (Left, Right), either)
import Data.Foreign (F)
import Data.Tuple (Tuple(Tuple))
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff(), later')
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX(), get, post, put, delete)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route(SongPage, SearchResultPage, UpdateSongPage, HomePage, NewSongPage, NotFoundPage, UpdateSongPage))
import Model (SearchResults, Song, serializeSong, parseSong)
import App (State(State), UIState(UIState), IOState(IOState), AsyncData(Loading, Loaded, LoadError))
import Text.Parsing.StringParser (runParser)


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route |
    Noop

data UIAction =
    SearchChange FormEvent |
    NewSongChange FormEvent |
    UpdateSongChange FormEvent

data IOAction =
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch String |
    ReceiveSearch (F SearchResults) |
    SubmitNewSong |
    SubmitUpdateSong |
    SubmitDeleteSong Int |
    ReceiveSubmitNewSong PostResponse |
    ReceiveSubmitUpdateSong PostResponse |
    ReceiveSubmitDeleteSong PostResponse

data PostResponse = Ok | Ko String

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
updatePage p@(SearchResultPage q) (State state) =
    updateIO (RequestSearch q) (State state { currentPage = p, ui = UIState { searchQuery: q } })
updatePage p@(UpdateSongPage _) (State state@{ io: IOState { song: song@Loaded(song'), searchResults, newSong  } }) =
    noEffects $ State state { currentPage = p, io = IOState { updateSong: Tuple (serializeSong song') (Right song'), song, searchResults, newSong } }
updatePage p (State state) = noEffects $ State state { currentPage = p }


--- UI Actions

updateUI :: UIAction -> State -> Affction

updateUI (SearchChange { target: { value } }) (State state) =
    noEffects $ State state { ui = UIState { searchQuery: value } }

updateUI (NewSongChange { target: { value } }) (State state@{ io: IOState { searchResults, song, updateSong } }) =
    noEffects $ State state { io = IOState { song, searchResults, updateSong, newSong: Tuple value (either (Left <<< show) Right $ runParser parseSong value) } }

updateUI (UpdateSongChange { target: { value } }) (State state@{ io: IOState { newSong, song, searchResults } }) =
    noEffects $ State state { io = IOState { song, searchResults, newSong, updateSong: Tuple value (either (Left <<< show) Right $ runParser parseSong value) } }

--- IO Actions

updateIO :: IOAction -> State -> Affction
updateIO (RequestSearch q) (State state@{ ui: UIState { searchQuery }, io: IOState { song, newSong, updateSong }}) = {
    state: State $ state { io = IOState { searchResults: Loading, song, newSong, updateSong } }
  , effects: [ do
        liftEff $ navigateTo $ "/search?q=" <> q
        res <- fetchSearch searchQuery
        let results = (readJSON res) :: F SearchResults
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIO (ReceiveSearch (Right r)) (State state@{ io: IOState { song, newSong, updateSong } }) =
    noEffects $ State $ state { io = IOState { searchResults: Loaded r, song, newSong, updateSong } }

updateIO (ReceiveSearch (Left e)) (State state@{ io: IOState { song, newSong, updateSong } }) =
    noEffects $ State $ state { io = IOState { searchResults: LoadError (show e), song, newSong, updateSong } }

updateIO (RequestSong id) (State state@{ io: IOState { searchResults, newSong, updateSong } }) = {
    state: State $ state { io = IOState { song: Loading, searchResults, newSong, updateSong } }
  , effects: [ do
        res <- fetchSong id
        let song = (readJSON res) :: F Song
        pure $ IOAction $ ReceiveSong song
    ]
}

updateIO (ReceiveSong (Right s)) (State state@{ io: IOState { searchResults, newSong, updateSong } }) =
    noEffects $ State state { io = IOState { song: Loaded s, searchResults, newSong, updateSong } }

updateIO (ReceiveSong (Left e)) (State state@{ io: IOState { searchResults, newSong, updateSong } }) =
    noEffects $ State state { io = IOState { song: LoadError (show e), searchResults, newSong, updateSong } }

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
        res <- updateSong id (PostSong s)
        pure $ IOAction $ ReceiveSubmitUpdateSong res
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


updateIO (ReceiveSubmitNewSong Ok) state = noEffects state
updateIO (ReceiveSubmitNewSong (Ko e)) (State state@{ io: IOState { searchResults, song, newSong: Tuple s _, updateSong } }) =
    noEffects $ State state { io = IOState { searchResults, song, newSong: Tuple s (Left e), updateSong } }

updateIO (ReceiveSubmitUpdateSong Ok) state = noEffects state
updateIO (ReceiveSubmitUpdateSong (Ko e)) (State state@{ io: IOState { searchResults, song, updateSong: Tuple s _, newSong } }) =
    noEffects $ State state { io = IOState { searchResults, song, updateSong: Tuple s (Left e), newSong } }

updateIO (ReceiveSubmitDeleteSong Ok) state = noEffects state
updateIO (ReceiveSubmitDeleteSong (Ko e)) (State state@{ io: IOState { searchResults, song, newSong: Tuple s _, updateSong } }) =
    noEffects $ State state { io = IOState { searchResults, song, newSong: Tuple s (Left e), updateSong } }

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

newtype PostSong = PostSong String

instance postSongEncodeJson :: EncodeJson PostSong where
    encodeJson (PostSong song)
        = "song" := song
        ~> jsonEmptyObject

postSong :: forall eff. PostSong -> Aff (ajax :: AJAX | eff) PostResponse
postSong s = do
    result <- post "/api/song" $ encodeJson s
    pure case result.status of
        (StatusCode 204) -> Ok
        _ -> Ko result.response

updateSong :: forall eff. Int -> PostSong -> Aff (ajax :: AJAX | eff) PostResponse
updateSong id s = do
    result <- put ("/api/song/" <> show id )  $ encodeJson s
    pure case result.status of
        (StatusCode 204) -> Ok
        _ -> Ko result.response

deleteSong :: forall eff. Int -> Aff (ajax :: AJAX | eff) PostResponse
deleteSong id = do
    result <- delete ("/api/song/" <> show id)
    pure case result.status of
        (StatusCode 204) -> Ok
        _ -> Ko result.response
