module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-), (==))

import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff(), later')
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route(SongPage, SearchResultPage))
import Model (SearchResults, Song)
import App (State(State), UIState(UIState), IOState(IOState), AsyncData(Loading, Loaded), HeaderVisibility(PendingHideHeader, HideHeader, ShowHeader))


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route |
    Noop

data UIAction =
    SearchChange FormEvent |
    UpdateHeaderVisibility |
    SetHideHeaderTimeout |
    SetShowHeader

data IOAction =
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch String |
    ReceiveSearch (F SearchResults)

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
updatePage p@(SearchResultPage q) (State state@{ ui: UIState { headerVisibility } }) =
    updateIO (RequestSearch q) (State state { currentPage = p, ui = UIState { searchQuery: q, headerVisibility } })
updatePage p (State state) = noEffects $ State state { currentPage = p }


--- UI Actions

updateUI :: UIAction -> State -> Affction

updateUI (SearchChange { target: { value } }) (State state@{ ui: UIState { headerVisibility } }) =
    noEffects $ State state { ui = UIState { searchQuery: value, headerVisibility } }

updateUI SetShowHeader (State state@{ ui: UIState { headerVisibility: HideHeader, searchQuery } }) = {
    state: State state { ui = UIState { headerVisibility: HideHeader, searchQuery } },
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateUI SetShowHeader (State state@{ ui: UIState { searchQuery } }) =
    noEffects $ State state { ui = UIState { headerVisibility: ShowHeader, searchQuery } }

updateUI UpdateHeaderVisibility (State state@{ ui: UIState { headerVisibility: PendingHideHeader, searchQuery } }) =
    noEffects $ State state { ui = UIState { headerVisibility: HideHeader, searchQuery } }

updateUI UpdateHeaderVisibility state = {
    state: state,
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

updateUI SetHideHeaderTimeout (State state@{ ui: UIState { searchQuery } }) = {
    state: State state { ui = UIState { headerVisibility: PendingHideHeader, searchQuery } },
    effects: [ do
        later' 3000 $ logShow ""
        pure $ UIAction UpdateHeaderVisibility
    ]
}

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

updateIO (ReceiveSong s) (State state@{ io: IOState { searchResults } }) = {
    state: State $ state { io = IOState { song: Loaded s, searchResults } },
    effects: [ do
        pure $ UIAction SetHideHeaderTimeout
    ]
}

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
