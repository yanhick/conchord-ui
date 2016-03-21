module Results where

import Prelude
import Data.Maybe
import Data.Functor.Coproduct (Coproduct())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Result as R

type List = Unit
data ListQuery a = ListQuery a
type State g = ParentState List R.State ListQuery R.Query g R.Slot
type Query = Coproduct ListQuery (ChildF R.Slot R.Query)

results :: forall g. (Functor g) => Component (State g) Query g
results = parentComponent { render, eval, peek: Just peek }
    where

        render :: List -> ParentHTML R.State ListQuery R.Query g R.Slot
        render st =
            H.div_ [H.h1_ [ H.text "Results" ]]

        eval :: Natural ListQuery (ParentDSL List R.State ListQuery R.Query g R.Slot)
        eval (ListQuery next) = pure next

        peek :: forall a. ChildF R.Slot R.Query a -> ParentDSL List R.State ListQuery R.Query g R.Slot Unit
        peek (ChildF p q) = case q of
            _ -> pure unit
