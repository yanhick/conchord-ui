module App where

import Prelude (id, const, ($), pure, bind, (<<<), show, (<$>), (<>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, form, input, button, text, div, ul, li)
import Pux.Html.Attributes (type_, value)
import Pux.Html.Events (FormEvent, onChange, onSubmit, onClick)
import Pux.Router (navigateTo, router, lit, int, end, param)
import Network.HTTP.Affjax (AJAX())
import Data.Foreign.Class (readJSON)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (F)
import Data.Functor ((<$))
import DOM (DOM())
import Model as M
import Api as A
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

data Action =
    SearchChange FormEvent |
    RequestDetail Int |
    ReceiveDetail (F M.Detail) |
    RequestSearch |
    ReceiveSearch (F M.List) |
    PageView Route

data Route = Home | SearchResult String | Detail Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
            Home <$ end
            <|>
            Detail <$> (lit "detail" *> int) <* end
            <|>
            SearchResult <$> (lit "search" *> (param "q")) <* end


type State = {
    q :: String
  , results :: M.List
  , detail :: Maybe M.Detail
  , currentPage :: Route
}

init = {
    q: ""
  , results: []
  , detail: Nothing
  , currentPage: Home
}

view :: State -> Html Action
view state =
    div []
        [form
            [ onSubmit (const RequestSearch) ]
            [ input [ type_ "text", value state.q, onChange SearchChange ] []
            , button [ type_ "submit" ] [ text "search" ]
            ]
        ,  ul [] ((\(M.Result r) -> li [] [ text r.title, button [ onClick (const $ RequestDetail r.id) ] [text $ show r.id] ]) <$> state.results)
        ,  div [] [ text (maybe "" (\(M.Result d) -> d.desc) state.detail) ]
        , page state.currentPage
        ]

page :: Route -> Html Action
page Home = div [] [ text "home" ]
page (SearchResult _) = div [] [ text "search result" ]
page (Detail _) = div [] [ text "detail page" ]
page _ = div [] [ text "not found" ]

update :: Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM)
update (PageView p) state = noEffects $ state { currentPage = p }
update (SearchChange ev) state = noEffects $ state { q = ev.target.value }
update RequestSearch state = {
    state: state
  , effects: [ do
        liftEff $ navigateTo $ "/search?q=" <> state.q
        res <- A.fetchResults state.q
        let results = (readJSON res) :: F M.List
        pure $ ReceiveSearch results
    ]
}
update (RequestDetail d) state = {
    state: state
    , effects: [ do
        liftEff $ navigateTo $ "/detail/" <> show d
        res <- A.fetchDetails d
        let results = (readJSON res) :: F M.Detail
        pure $ ReceiveDetail results
    ]
}
update (ReceiveSearch (Right r)) state = noEffects $ state { results = r }
update (ReceiveDetail (Right d)) state = noEffects $ state { detail = Just d }
update _ state = noEffects $ state
