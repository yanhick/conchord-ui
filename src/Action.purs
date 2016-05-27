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
    SearchChange FormEvent |
    UIAction UIAction |
    PageView Route

data UIAction =
    Increment |
    Decrement

data IOAction =
    RequestDetail Int |
    ReceiveDetail (F Detail) |
    RequestSearch |
    ReceiveSearch (F List)


update :: Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)
update (PageView p) state = noEffects $ state { currentPage = p }
update (SearchChange ev) state = noEffects $ state { q = ev.target.value }
update (IOAction a) state = updateIOAction a state

update (UIAction Increment) state = noEffects $ state { fontSize = state.fontSize + 1.0 }
update (UIAction Decrement) state = noEffects $ state { fontSize = state.fontSize - 1.0 }

--- IO Actions

updateIOAction :: IOAction -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)

updateIOAction RequestSearch state = {
    state: state { detail = Nothing, results = [] }
  , effects: [ do
        liftEff $ navigateTo $ "/search/?q=" <> state.q
        res <- fetchResults state.q
        let results = (readJSON res) :: F List
        pure $ IOAction $ ReceiveSearch results
    ]
}

updateIOAction (RequestDetail d) state = {
    state: state
    , effects: [ do
        liftEff $ navigateTo $ "/detail/" <> show d
        res <- fetchDetails d
        let results = (readJSON res) :: F Detail
        pure $ IOAction $ ReceiveDetail results
    ]
}

updateIOAction (ReceiveSearch (Right r)) state = noEffects $ state { results = r }
updateIOAction (ReceiveSearch (Left _)) state = noEffects state

updateIOAction (ReceiveDetail (Right d)) state = noEffects $ state { detail = Just d }
updateIOAction (ReceiveDetail (Left _)) state = noEffects state

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
