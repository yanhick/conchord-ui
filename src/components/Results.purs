module Results where

import Prelude
import Data.Maybe
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Result as R

type List = { q :: String }
data ListQuery a = ListQuery a
type State g = ParentState List R.State ListQuery R.Query g Slot
type Query = Coproduct ListQuery (ChildF Slot R.Query)

data Slot = Slot String

derive instance genericSlot :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

initState :: List
initState = { q: "" }

results :: forall g. (Functor g) => Component (State g) Query g
results = parentComponent { render, eval, peek: Just peek }
    where

        render :: List -> ParentHTML R.State ListQuery R.Query g Slot
        render st =
            H.div_ [H.h1_ [ H.text "Results" ]]

        eval :: Natural ListQuery (ParentDSL List R.State ListQuery R.Query g Slot)
        eval (ListQuery next) = pure next

        peek :: forall a. ChildF Slot R.Query a -> ParentDSL List R.State ListQuery R.Query g Slot Unit
        peek (ChildF p q) = case q of
            _ -> pure unit
