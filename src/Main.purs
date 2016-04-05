module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Generic (class Generic, gEq, gCompare)
import Data.Either (Either(Left))
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)

import Halogen (HalogenEffects, ParentDSL, Natural,
               ParentHTML, Component, ParentState, ChildF(ChildF),
               parentState, runUI, parentComponent, modify, request, query', action
               )
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Indexed as H
import Search as S
import Results as R
import Result as Re
import Detail as D

type State = { label :: String, selected :: Maybe R.ResultSlot }

data ListSlot = ListSlot
data DetailSlot = DetailSlot
data SearchSlot = SearchSlot

derive instance genericListSlot :: Generic ListSlot
instance eqListSlot :: Eq ListSlot where eq = gEq
instance ordListSlot :: Ord ListSlot where compare = gCompare

derive instance genericDetailSlot :: Generic DetailSlot
instance eqDetailSlot :: Eq DetailSlot where eq = gEq
instance ordDetailSlot :: Ord DetailSlot where compare = gCompare

derive instance genericSearchSlot :: Generic SearchSlot
instance eqSearchSlot :: Eq SearchSlot where eq = gEq
instance ordSearchSlot :: Ord SearchSlot where compare = gCompare

initialState :: State
initialState = { label: "", selected: Nothing }

data Query a = ReadStates a

type ChildState g = Either S.State (Either (R.State g) D.State)
type ChildQuery = Coproduct S.Query (Coproduct R.Query D.Query)
type ChildSlot = Either SearchSlot (Either ListSlot DetailSlot)

cpSearch :: forall g. ChildPath S.State (ChildState g) S.Query ChildQuery SearchSlot ChildSlot
cpSearch = cpL

cpResults :: forall g. ChildPath (R.State g) (ChildState g)  R.Query ChildQuery ListSlot ChildSlot
cpResults = cpR :> cpL

cpDetail :: forall g. ChildPath D.State (ChildState g)  D.Query ChildQuery DetailSlot ChildSlot
cpDetail = cpR :> cpR


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
            , H.slot' cpSearch SearchSlot \_ -> { component: S.search, initialState: S.initState}
            , H.slot' cpResults ListSlot \_ -> { component: R.results, initialState: (parentState (R.List { resultIds: [0, 1], selected: st.selected })) }
            , H.slot' cpDetail DetailSlot \_ -> { component: D.detail , initialState: ({title: st.selected, desc:"sadf", id:3}) }
            ]

    eval :: Natural Query (ParentDSL State (ChildState g) Query ChildQuery g ChildSlot)
    eval (ReadStates next) = pure next

    peek :: forall a. ChildF ChildSlot ChildQuery a -> PeekP g
    peek (ChildF p q) = coproduct (peekSearch p) peekListAndDetail q

    peekSearch :: forall a. ChildSlot -> S.Query a -> PeekP g
    peekSearch (Left p) (S.Submit _) = do
        search <- query' cpSearch p (request S.GetQuery)
        modify \st -> st {label = fromMaybe "" search}
    peekSearch _ _ = pure unit

    peekListAndDetail :: forall a. Coproduct R.Query D.Query a -> PeekP g
    peekListAndDetail = coproduct peekResults (const (pure unit))

    peekResults :: forall a. R.Query a -> PeekP g
    peekResults = coproduct (const (pure unit)) peekResult

    peekResult :: forall a. ChildF R.ResultSlot Re.Query a -> PeekP g
    peekResult (ChildF p (Re.Select _)) = do
        modify \st -> st {selected = Just p}
        query' cpDetail DetailSlot (action (D.SetTitle $ show p))
        pure unit


main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    runUI ui (parentState initialState) body
