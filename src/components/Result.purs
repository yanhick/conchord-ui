module Result where

import Prelude
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H


type State = { id :: Int, title :: String, desc :: String }
data Query a = Select a

newtype Slot = Slot Int

derive instance genericSlot :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

initState :: State
initState = { title: "Hello", desc: "world", id: 1 }

result :: forall g. (Functor g) => Component State Query g
result = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render r =
            H.li_ [ H.text r.title ]

        eval :: Natural Query (ComponentDSL State Query g)
        eval (Select next) = pure next

