module Result where

import Prelude

import Halogen (ComponentDSL, Natural, ComponentHTML, Component, component)

import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Model as M


data Query a = Select a

initState :: Int -> M.Result
initState id = M.Result { title: "Hello", desc: "world", id: id }

result :: forall g. (Functor g) => Component M.Result Query g
result = component { render, eval }
    where

        render :: M.Result -> ComponentHTML Query
        render (M.Result r) =
            H.li_
                [ H.h2_ [ H.text (show r.id) ]
                , H.h3_ [ H.text (r.title) ]
                , H.p_  [ H.text (r.desc) ]
                , H.button
                    [ E.onClick (E.input_ Select) ]
                    [ H.text "select" ]
                ]

        eval :: Natural Query (ComponentDSL M.Result Query g)
        eval (Select next) = pure next

