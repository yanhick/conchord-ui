module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-), (==))

import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())
import Global (decodeURIComponent)

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route(SongPage, SearchResultPage))
import Model (SearchResults, Song)
import App (State(State), UIState(UIState), IOState(IOState), AsyncData(Loading, Loaded))


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route

data UIAction = SearchChange FormEvent

data IOAction =
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch String |
    ReceiveSearch (F SearchResults)

type Affction = EffModel State Action (ajax :: AJAX, dom :: DOM)


update :: Action -> State -> Affction
update (PageView p@(SongPage s)) (State state) =
    updateIO (RequestSong s) (State (state { currentPage = p }))
update (PageView p@(SearchResultPage q)) (State state@{ currentPage: (SearchResultPage q') })
    | q == q' = noEffects $ State state { ui = UIState { searchQuery: decodeURIComponent q } }
update (PageView p@(SearchResultPage q)) (State state) =
    updateIO (RequestSearch q) (State state { currentPage = p, ui = UIState { searchQuery: decodeURIComponent q } })
update (PageView p) (State state) = noEffects $ State state { currentPage = p }
update (IOAction a) state = updateIO a state
update (UIAction a) (State state@{ ui }) = noEffects $ State state { ui = updateUI a ui }

--- UI Actions

updateUI :: UIAction -> UIState -> UIState
updateUI (SearchChange ev) (UIState state) = UIState state { searchQuery = ev.target.value }

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

updateIO (ReceiveSearch r) (State state@{ io: IOState { song } }) =
    noEffects $ State $ state { io = IOState { searchResults: Loaded r, song: song } }

updateIO (RequestSong id) (State state@{ io: IOState { searchResults } }) = {
    state: State $ state { io = IOState { song: Loading, searchResults } }
  , effects: [ do
        res <- fetchSong id
        let song = (readJSON res) :: F Song
        pure $ IOAction $ ReceiveSong song
    ]
}

updateIO (ReceiveSong s) (State state@{ io: IOState { searchResults } }) =
    noEffects $ State $ state { io = IOState { song: Loaded s, searchResults } }

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
