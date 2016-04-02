module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Functor.Coproduct
import Data.Generic (Generic, gEq, gCompare)
import Data.Either
import Data.Maybe

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Indexed as H
import qualified Search as S
import qualified Results as R
import qualified Result as Re

type State = { label :: String, selected :: Maybe R.ResultSlot }

data ListSlot = ListSlot

derive instance genericSlot :: Generic ListSlot
instance eqSlot :: Eq ListSlot where eq = gEq
instance ordSlot :: Ord ListSlot where compare = gCompare


initialState :: State
initialState = { label: "", selected: Nothing }

data Query a = ReadStates a

type ChildState g = Either S.State (R.State g)
type ChildQuery = Coproduct S.Query R.Query
type ChildSlot = Either S.Slot ListSlot

cpSearch :: forall g. ChildPath S.State (ChildState g) S.Query ChildQuery S.Slot ChildSlot
cpSearch = cpL

cpResults :: forall g. ChildPath (R.State g) (ChildState g)  R.Query ChildQuery ListSlot ChildSlot
cpResults = cpR


type StateP g = ParentState State (ChildState g) Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type PeekP g = ParentDSL State (ChildState g) Query ChildQuery g ChildSlot Unit

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Just peek }
    where

    render :: State -> ParentHTML (ChildState g) Query ChildQuery g ChildSlot
    render st =
        H.div_
            [ H.div_
                [
                    H.text st.label
                ]
            , H.div_
                [
                    H.text $ show st.selected
                ]
            , H.slot' cpSearch (S.Slot 0) \_ -> { component: S.search, initialState: S.initState}
            , H.slot' cpResults ListSlot \_ -> { component: R.results, initialState: (parentState (R.List { resultIds: [0, 1], selected: st.selected })) }
            ]

    eval :: Natural Query (ParentDSL State (ChildState g) Query ChildQuery g ChildSlot)
    eval (ReadStates next) = pure next

    peek :: forall a. ChildF ChildSlot ChildQuery a -> PeekP g
    peek (ChildF p q) = coproduct (peekSearch p) peekResults q

    peekSearch :: forall a. ChildSlot -> S.Query a -> PeekP g
    peekSearch (Left p) (S.Submit _) = do
        search <- query' cpSearch p (request S.GetQuery)
        modify \st -> st {label = fromMaybe "" search}
    peekSearch _ _ = pure unit

    peekResults :: forall a. R.Query a -> PeekP g
    peekResults = coproduct (const (pure unit)) peekResult

    peekResult :: forall a. ChildF R.ResultSlot Re.Query a -> PeekP g
    peekResult (ChildF p (Re.Select _)) = modify \st -> st {selected = Just p}


main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    runUI ui (parentState initialState) body
