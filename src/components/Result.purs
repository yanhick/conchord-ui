module Result where

import Prelude
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E


type State = { id :: Int, title :: String, desc :: String }
data Query a = Select a

initState :: State
initState = { title: "Hello", desc: "world", id: 1 }

result :: forall g. (Functor g) => Component State Query g
result = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render r =
            H.li_
                [ H.h2_
                    [ H.text (show r.id) ]
                , H.button
                    [ E.onClick (E.input_ Select) ]
                    [ H.text "select" ]
                ]

        eval :: Natural Query (ComponentDSL State Query g)
        eval (Select next) = pure next

