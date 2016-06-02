module Action where

import Prelude (($), bind, (<>), pure, show, (+), (-))

import Data.Maybe (Maybe(Nothing, Just))
import Data.Either (Either(Right, Left))
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM())

import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)
import Pux.Router (navigateTo)

import Route (Route())
import Model (SearchResults, Song, song)
import App (State, UIState)


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route

data UIAction =
    SearchChange FormEvent |
    Increment |
    Decrement

data IOAction =
    RequestSong Int |
    ReceiveSong (F Song) |
    RequestSearch |
    ReceiveSearch (F SearchResults)

type Affction = EffModel State Action (ajax :: AJAX, dom :: DOM)


update :: Action -> State -> Affction
update (PageView p) state = noEffects $ state { currentPage = p }
update (IOAction a) state = updateIO a state
update (UIAction a) state = noEffects $ state { ui = updateUI a state.ui }

--- UI Actions

updateUI :: UIAction -> UIState -> UIState
updateUI (SearchChange ev) state = state { searchQuery = ev.target.value }
updateUI Increment state = state { songFontSize = state.songFontSize + 1.0 }
updateUI Decrement state = state { songFontSize = state.songFontSize - 1.0 }

--- IO Actions

updateIO :: IOAction -> State -> Affction

updateIO RequestSearch state = {
    state: state { io = state.io { searchResults = [] } }
  , effects: [ do
        liftEff $ navigateTo $ "/search/?q=" <> state.ui.searchQuery
        res <- fetchSearch state.ui.searchQuery
        let results = (readJSON res) :: F SearchResults
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIO (ReceiveSearch (Right r)) state = noEffects $ state { io = state.io { searchResults = r } }
updateIO (ReceiveSearch (Left _)) state = noEffects state

updateIO (RequestSong id) state = {
    state: state { io = state.io { song = Nothing } }
  , effects: [ do
        liftEff $ navigateTo $ "/song/" <> show id
        res <- fetchSong id
        let song = (readJSON res) :: F Song
        pure $ IOAction $ ReceiveSong song
    ]
}

updateIO (ReceiveSong s) state = noEffects $ state { io = state.io { song = pure s } }

--- AJAX Requests

fetchSearch :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchSearch q = do
    result <- get $ "/search?q=" <> q
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"

fetchSong :: forall eff. Int -> Aff (ajax :: AJAX | eff) String
fetchSong id = do
    result <- get $ "/song/" <> show id
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"
