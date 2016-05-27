module App where

import Prelude (const, ($), pure, bind, show, (<>), (+), (-))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div)
import Pux.Router (navigateTo)
import Network.HTTP.Affjax (AJAX())
import Data.Foreign.Class (readJSON)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (F)
import DOM (DOM())
import Model as M
import Api (fetchResults, fetchDetails)
import View (page)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Action (Action(..), UIAction(..))
import Route (Route (..))
import Model (State)


view :: State -> Html Action
view state = div [] [ page state.currentPage state ]

update :: Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)
update (PageView p) state = noEffects $ state { currentPage = p }
update (SearchChange ev) state = noEffects $ state { q = ev.target.value }
update RequestSearch state = {
    state: state { detail = Nothing, results = [] }
  , effects: [ do
        liftEff $ navigateTo $ "/search/?q=" <> state.q
        res <- fetchResults state.q
        let results = (readJSON res) :: F M.List
        pure $ ReceiveSearch results
    ]
}
update (RequestDetail d) state = {
    state: state
    , effects: [ do
        liftEff $ navigateTo $ "/detail/" <> show d
        res <- fetchDetails d
        let results = (readJSON res) :: F M.Detail
        pure $ ReceiveDetail results
    ]
}
update (ReceiveSearch (Right r)) state = noEffects $ state { results = r }
update (ReceiveSearch (Left _)) state = noEffects state

update (ReceiveDetail (Right d)) state = noEffects $ state { detail = Just d }
update (ReceiveDetail (Left _)) state = noEffects state

update (UIAction Increment) state = noEffects $ state { fontSize = state.fontSize + 1.0 }
update (UIAction Decrement) state = noEffects $ state { fontSize = state.fontSize - 1.0 }
