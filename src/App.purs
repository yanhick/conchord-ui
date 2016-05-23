module App where

import Prelude (const, ($), pure, bind, show, (<$>), (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, form, input, button, text, div, ul, li)
import Pux.Html.Attributes (type_, value)
import Pux.Html.Events (onChange, onSubmit, onClick)
import Pux.Router (navigateTo)
import Network.HTTP.Affjax (AJAX())
import Data.Foreign.Class (readJSON)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (F)
import DOM (DOM())
import Model as M
import Api (fetchResults, fetchDetails)
import Song as S
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Action (Action(..))
import Route (Route (..))


type State = {
    q :: String
  , results :: M.List
  , detail :: Maybe M.Detail
  , song :: Maybe M.Song
  , currentPage :: Route
}

init :: State
init = {
    q: ""
  , results: []
  , detail: Nothing
  , song: Just M.song
  , currentPage: Home
}

view :: State -> Html Action
view state = div [] [ page state.currentPage state ]

page :: Route -> State -> Html Action
page (Detail _) state = div [] [ text (maybe "" (\(M.Result d) -> d.desc) state.detail), S.view state.song ]
page (SearchResult _) state = ul [] ((\(M.Result r) -> li [] [ text r.title, button [ onClick (const $ RequestDetail r.id) ] [text $ show r.id] ]) <$> state.results)
page Home state =
    div []
        [form
            [ onSubmit (const RequestSearch) ]
            [ input [ type_ "text", value state.q, onChange SearchChange ] []
            , button [ type_ "submit" ] [ text "search" ]
            ]
        ]
page _ _ = div [] [ text "not found" ]

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
update (ReceiveDetail (Right d)) state = noEffects $ state { detail = Just d }
update _ state = noEffects $ state
