module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-), (==))

import Data.Maybe (Maybe (Just))
import Data.Argonaut (class EncodeJson, (~>), (:=), jsonEmptyObject, encodeJson)
import Data.Either (Either (Left, Right))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff(), later')
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX(), get, post)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route(SongPage, SearchResultPage))
import Model (SearchResults, Song)
import App (State(State), UIState(UIState), IOState(IOState), AsyncData(Loading, Loaded, LoadError), HeaderVisibility(PendingHideHeader, HideHeader, ShowHeader))


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route |
    Noop

data UIAction =
    SearchChange FormEvent |
    UpdateHeaderVisibility |
    NewSongChange FormEvent |
    SetHideHeaderTimeout |
    SetShowHeader

data IOAction =
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch String |
    ReceiveSearch (F SearchResults) |
    SubmitNewSong |
    ReceiveSubmitNewSong PostResponse

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
updatePage p@(SearchResultPage q) (State state@{ ui: UIState { headerVisibility, newSong } }) =
    updateIO (RequestSearch q) (State state { currentPage = p, ui = UIState { searchQuery: q, headerVisibility, newSong } })
updatePage p (State state) = noEffects $ State state { currentPage = p }


--- UI Actions

updateUI :: UIAction -> State -> Affction

updateUI (SearchChange { target: { value } }) (State state@{ ui: UIState { headerVisibility, newSong } }) =
    noEffects $ State state { ui = UIState { searchQuery: value, headerVisibility, newSong } }

updateUI SetShowHeader (State state@{ ui: UIState { headerVisibility: HideHeader, searchQuery, newSong } }) = {
    state: State state { ui = UIState { headerVisibility: HideHeader, searchQuery, newSong } },
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateUI SetShowHeader (State state@{ ui: UIState { searchQuery, newSong } }) =
    noEffects $ State state { ui = UIState { headerVisibility: ShowHeader, searchQuery, newSong } }

updateUI UpdateHeaderVisibility (State state@{ ui: UIState { headerVisibility: PendingHideHeader, searchQuery, newSong } }) =
    noEffects $ State state { ui = UIState { headerVisibility: HideHeader, searchQuery, newSong } }

updateUI UpdateHeaderVisibility state = {
    state: state,
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateUI SetHideHeaderTimeout (State state@{ ui: UIState { searchQuery, newSong } }) = {
    state: State state { ui = UIState { headerVisibility: PendingHideHeader, searchQuery, newSong } },
    effects: [ do
        later' 3000 $ logShow ""
        pure $ UIAction UpdateHeaderVisibility
    ]
}

updateUI (NewSongChange { target: { value } }) (State state@{ ui: UIState { headerVisibility, searchQuery} }) =
    noEffects $ State state { ui = UIState { searchQuery, headerVisibility, newSong: value } }

--- IO Actions

updateIO :: IOAction -> State -> Affction
updateIO (RequestSearch q) (State state@{ ui: UIState { searchQuery }, io: IOState { song }}) = {
    state: State $ state { io = IOState { searchResults: Loading, song: song } }
  , effects: [ do
        liftEff $ navigateTo $ "/search?q=" <> q
        res <- fetchSearch searchQuery
        let results = (readJSON res) :: F SearchResults
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIO (ReceiveSearch (Right r)) (State state@{ io: IOState { song } }) =
    noEffects $ State $ state { io = IOState { searchResults: Loaded r, song: song } }

updateIO (ReceiveSearch (Left e)) (State state@{ io: IOState { song } }) =
    noEffects $ State $ state { io = IOState { searchResults: LoadError (show e), song: song } }

updateIO (RequestSong id) (State state@{ io: IOState { searchResults } }) = {
    state: State $ state { io = IOState { song: Loading, searchResults } }
  , effects: [ do
        res <- fetchSong id
        let song = (readJSON res) :: F Song
        pure $ IOAction $ ReceiveSong song
    ]
}

updateIO (ReceiveSong (Right s)) (State state@{ io: IOState { searchResults } }) = {
    state: State $ state { io = IOState { song: Loaded s, searchResults } },
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateIO (ReceiveSong (Left e)) (State state@{ io: IOState { searchResults } }) = {
    state: State $ state { io = IOState { song: LoadError (show e), searchResults } },
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateIO SubmitNewSong state@(State { ui: UIState { newSong } })= {
    state: state,
    effects: [ do
        res <- postNewSong (PostNewSong newSong)
        pure $ IOAction $ ReceiveSubmitNewSong res
    ]
}

updateIO (ReceiveSubmitNewSong Ok) state = noEffects state
updateIO (ReceiveSubmitNewSong (Ko e)) (State state@{ ui: UIState { headerVisibility, searchQuery} }) =
    noEffects $ State state { ui = UIState { searchQuery, headerVisibility, newSong: e } }

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

newtype PostNewSong = PostNewSong String

instance postNewSongEncodeJson :: EncodeJson PostNewSong where
    encodeJson (PostNewSong song)
        = "song" := song
        ~> jsonEmptyObject

postNewSong :: forall eff. PostNewSong -> Aff (ajax :: AJAX | eff) PostResponse
postNewSong s = do
    result <- post "/api/song" $ encodeJson s
    pure case result.status of
        (StatusCode 204) -> Ok
        _ -> Ko result.response
