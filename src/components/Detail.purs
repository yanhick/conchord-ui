module Detail where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (readProp)
import Data.Either (Either (Left, Right))
import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component, component, modify, fromAff)
import Halogen.HTML.Indexed as H
import Results as R
import Control.Monad.Aff (Aff())
import Data.Maybe (Maybe (Nothing))

import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))

type State = { id :: Maybe Int, title :: Maybe R.ResultSlot, desc :: String }
data Query a = Query a | Load (Maybe Int) a

type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

initState :: State
initState = { title: Nothing, desc: "world", id: Nothing }

detail :: forall eff. Component State Query (Aff (AppEffects eff))
detail = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render d =
            H.div_ [ H.h2_ [H.text ("Selected:" <> show d.id)]
                    , H.h3_ [H.text ("fetched: " <> d.desc)]
                    ]

        eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
        eval (Query next) = pure next
        eval (Load id next) = do
            modify (_ { id = id })
            result <- fromAff fetch
            modify (_ { desc = result })
            pure next


fetch :: forall eff. Aff (ajax :: AJAX | eff) String
fetch = do
    result <- get "http://localhost:1337"
    return case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"
