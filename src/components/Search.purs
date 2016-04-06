module Search where

import Prelude

import Data.Generic (class Generic, gEq, gCompare)

import Halogen (ComponentDSL, Natural, ComponentHTML, Component, component, gets, modify, action)
import Data.Functor (($>))
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E
import Model as M

data Query a = Submit a 
             | Change String a
             | GetQuery (String -> a)

initState :: M.Search
initState = { q: "" }

search :: forall g. (Functor g) => Component M.Search Query g
search = component { render, eval }
    where

    render :: M.Search -> ComponentHTML Query
    render st =
        H.form
        [ E.onSubmit \_ -> EH.preventDefault $> action Submit ]
        [ H.input [ P.inputType P.InputText
                  , P.placeholder st.q
                  , E.onValueChange (E.input Change)
                  ]
                  , H.input [ P.inputType P.InputSubmit ]
        ]

    eval :: Natural Query (ComponentDSL M.Search Query g)
    eval ( Submit next ) = pure next
    eval ( Change desc next ) = do
        modify (\st -> st { q = desc })
        pure next
    eval ( GetQuery continue ) = do
        q <- gets (\st -> st.q)
        pure (continue q)
