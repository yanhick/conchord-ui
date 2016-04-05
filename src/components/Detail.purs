module Detail where

import Prelude

import Halogen (ComponentDSL, Natural, ComponentHTML, Component, component, modify)
import Halogen.HTML.Indexed as H
import Results as R
import Data.Maybe (Maybe (Nothing))


type State = { id :: Int, title :: Maybe R.ResultSlot, desc :: String }
data Query a = Query a | SetTitle String a

initState :: State
initState = { title: Nothing, desc: "world", id: 1 }

detail :: forall g. (Functor g) => Component State Query g
detail = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render d =
            H.div_ [ H.text ("Selected:" <> show d.desc) ]

        eval :: Natural Query (ComponentDSL State Query g)
        eval (Query next) = pure next
        eval (SetTitle desc next) = do
            modify (_ { desc = desc })
            pure next

