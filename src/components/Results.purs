module Results where

import Prelude
import Data.Maybe (Maybe (Just, Nothing))
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (class Generic, gEq, gCompare)

import Halogen (ParentDSL, Natural, ParentHTML, Component, ParentState, ChildF(ChildF), parentComponent, modify)
import Halogen.HTML.Indexed as H
import Result as R
import Model as M

type StateP g = ParentState M.List M.Result Query R.Query g ResultSlot
type QueryP = Coproduct Query (ChildF ResultSlot R.Query)

data Query a = Query a

newtype ResultSlot = ResultSlot Int

derive instance genericSlot :: Generic ResultSlot
instance eqSlot :: Eq ResultSlot where eq = gEq
instance ordSlot :: Ord ResultSlot where compare = gCompare

instance showSlot :: Show ResultSlot where
    show (ResultSlot i) = show i

initState :: M.List
initState = []

results :: forall g. (Functor g) => Component (StateP g) QueryP g
results = parentComponent { render, eval, peek: Nothing }
    where

        render :: M.List -> ParentHTML M.Result Query R.Query g ResultSlot
        render st =
            H.div_ [ H.h1_ [ H.text "Results" ]
                   , H.ul_
                        (map renderResult st)
                   ]

        renderResult :: M.Result -> ParentHTML M.Result Query R.Query g ResultSlot
        renderResult { id }=
            H.li_
                [ H.slot (ResultSlot id) \_ ->
                    { component: R.result, initialState: (R.initState { id = id })}
                ]

        eval :: Natural Query (ParentDSL M.List M.Result Query R.Query g ResultSlot)
        eval (Query next) = pure next
