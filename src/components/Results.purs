module Results where

import Prelude
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (class Generic, gEq, gCompare)
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Class (liftEff)

import Halogen (ParentDSL, Natural, ParentHTML, Component, ParentState, ChildF(ChildF), parentComponent, modify, get, query, action, fromEff, liftH)
import Halogen.HTML.Indexed as H
import Result as R
import Model as M

type State = Maybe M.List
type StateP g = ParentState State M.Result Query R.Query g ResultSlot
type QueryP = Coproduct Query (ChildF ResultSlot R.Query)

data Query a = SetResults State a

newtype ResultSlot = ResultSlot Int

derive instance genericSlot :: Generic ResultSlot
instance eqSlot :: Eq ResultSlot where eq = gEq
instance ordSlot :: Ord ResultSlot where compare = gCompare

instance showSlot :: Show ResultSlot where
    show (ResultSlot i) = show i

initState :: State
initState = Nothing

results :: forall g. (Functor g) => Component (StateP g) QueryP g
results = parentComponent { render, eval, peek: Nothing }
    where

        render :: State -> ParentHTML M.Result Query R.Query g ResultSlot
        render st =
            H.div_ [ H.h1_ [ H.text "Results" ]
                   , H.ul_
                        (renderResult <$> fromMaybe [] st)
                   ]

        renderResult :: M.Result -> ParentHTML M.Result Query R.Query g ResultSlot
        renderResult r@(M.Result { id }) =
            H.li_
                [ H.slot (ResultSlot id) \_ -> { component: R.result, initialState: r } ]

        eval :: Natural Query (ParentDSL State M.Result Query R.Query g ResultSlot)
        eval (SetResults r next) = do
            modify \_ -> r
            pure next
