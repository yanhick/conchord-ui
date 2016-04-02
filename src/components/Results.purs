module Results where

import Prelude
import Data.Maybe
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Result as R

newtype List = List { resultIds :: Array Int, selected :: Maybe ResultSlot }
type State g = ParentState List R.State ListQuery R.Query g ResultSlot
type Query = Coproduct ListQuery (ChildF ResultSlot R.Query)

data ListQuery a = GetSelected (Maybe ResultSlot -> a)

newtype ResultSlot = ResultSlot Int

derive instance genericSlot :: Generic ResultSlot
instance eqSlot :: Eq ResultSlot where eq = gEq
instance ordSlot :: Ord ResultSlot where compare = gCompare

instance showSlot :: Show ResultSlot where
    show (ResultSlot i) = show i

initState :: List
initState = List { resultIds: [0, 1], selected: Nothing }

results :: forall g. (Functor g) => Component (State g) Query g
results = parentComponent { render, eval, peek: Just peek }
    where

        render :: List -> ParentHTML R.State ListQuery R.Query g ResultSlot
        render (List st) =
            H.div_ [ H.h1_ [ H.text "Results" ]
                   , H.h2_ [ H.text ("Selected: " <> show st.selected) ]
                   , H.ul_
                        (map renderResult st.resultIds)
                   ]

        renderResult :: Int -> ParentHTML R.State ListQuery R.Query g ResultSlot
        renderResult resultId =
            H.li_
                [ H.slot (ResultSlot resultId) \_ ->
                    { component: R.result, initialState: (R.initState { id = resultId })}
                ]

        eval :: Natural ListQuery (ParentDSL List R.State ListQuery R.Query g ResultSlot)
        eval (GetSelected continue) = do
            pure (continue Nothing)

        peek :: forall a. ChildF ResultSlot R.Query a -> ParentDSL List R.State ListQuery R.Query g ResultSlot Unit
        peek (ChildF p q) = case q of
            R.Select _ -> modify (\(List st) -> List $ st { selected = Just p })
            _ -> pure unit
