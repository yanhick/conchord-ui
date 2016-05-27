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
import Model (Detail, List, State)


data Action =
    IOAction IOAction |
    UIAction UIAction |
    PageView Route

data UIAction =
    SearchChange FormEvent |
    Increment |
    Decrement

data IOAction =
    RequestDetail Int |
    ReceiveDetail (F Detail) |
    RequestSearch |
    ReceiveSearch (F List)


update :: Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)
update (PageView p) state = noEffects $ state { currentPage = p }
update (IOAction a) state = updateIO a state
update (UIAction a) state = updateUI a state

--- UI Actions

updateUI :: UIAction -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)
updateUI (SearchChange ev) state = noEffects $ state { q = ev.target.value }
updateUI Increment state = noEffects $ state { fontSize = state.fontSize + 1.0 }
updateUI Decrement state = noEffects $ state { fontSize = state.fontSize - 1.0 }

--- IO Actions

updateIO :: IOAction -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)

updateIO RequestSearch state = {
    state: state { detail = Nothing, results = [] }
  , effects: [ do
        liftEff $ navigateTo $ "/search/?q=" <> state.q
        res <- fetchResults state.q
        let results = (readJSON res) :: F List
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIO (RequestDetail d) state = {
    state: state
    , effects: [ do
        liftEff $ navigateTo $ "/detail/" <> show d
        res <- fetchDetails d
        let results = (readJSON res) :: F Detail
        pure $ IOAction $ ReceiveDetail results
    ]
}

updateIO (ReceiveSearch (Right r)) state = noEffects $ state { results = r }
updateIO (ReceiveSearch (Left _)) state = noEffects state

updateIO (ReceiveDetail (Right d)) state = noEffects $ state { detail = Just d }
updateIO (ReceiveDetail (Left _)) state = noEffects state

--- AJAX Requests

fetchResults :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchResults q = do
    result <- get $ "http://localhost:8080/results?q=" <> q
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"

fetchDetails :: forall eff. Int -> Aff (ajax :: AJAX | eff) String
fetchDetails id = do
    result <- get $ "http://localhost:8080/details/" <> show id
    pure case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"
