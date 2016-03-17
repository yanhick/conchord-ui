module Search where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Data.Functor (($>))
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

type State = { q :: String }
data Query a = Submit a | Change String a

data Slot = Slot

derive instance genericSlot :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

initState :: State
initState = { q: "" }

search :: forall g. (Functor g) => Component State Query g
search = component render eval
    where

    render :: State -> ComponentHTML Query
    render st =
        H.form
        [ E.onSubmit \_ -> EH.preventDefault $> action Submit ]
        [ H.input [ P.inputType P.InputSearch
                  , P.value st.q
                  , E.onValueChange (E.input Change)
                  ]
                  , H.input [ P.inputType P.InputSubmit ]
        ]

    eval :: Natural Query (ComponentDSL State Query g)
    eval ( Submit next ) = pure next
    eval ( Change desc next ) = do
        modify (\state -> state { q = desc })
        pure next
