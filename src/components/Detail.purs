module Detail where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H


type State = { id :: Int, title :: String, desc :: String }
data Query a = Query a

initState :: State
initState = { title: "Hello", desc: "world", id: 1 }

detail :: forall g. (Functor g) => Component State Query g
detail = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render d =
            H.li_ [ H.text $ show d.id ]

        eval :: Natural Query (ComponentDSL State Query g)
        eval (Query next) = pure next

