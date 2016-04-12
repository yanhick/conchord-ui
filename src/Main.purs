module Main where

import Prelude

import Data.Functor ((<$))
import Routing
import Routing.Match
import Routing.Match.Class
import Control.Apply ((<*))
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Data.Functor.Coproduct (Coproduct, coproduct, left)
import Data.Generic (class Generic, gEq, gCompare)
import Data.Either (Either(Left))
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), post)
import Data.Tuple (Tuple(Tuple))
import DOM
import Control.Monad.Aff.AVar
import Control.Monad.Eff.Exception

import Halogen (HalogenEffects, ParentDSL, Natural,
               ParentHTML, Component, ParentState, ChildF(ChildF),
               parentState, runUI, parentComponent, modify, request, query', action,
               Driver
               )
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Indexed as H
import Search as S
import Results as R
import Result as Re
import Detail as D
import Model as M
import DummyData as DD
import Routing.Match (Match)

type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)
type Affect eff = Aff (AppEffects eff)

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

data Query a = Goto Routes a

type ChildState g = Either M.Search (Either (R.StateP g) D.State)
type ChildQuery = Coproduct S.Query (Coproduct R.QueryP D.Query)
type ChildSlot = Either SearchSlot (Either ListSlot DetailSlot)

cpSearch :: forall g. ChildPath M.Search (ChildState g) S.Query ChildQuery SearchSlot ChildSlot
cpSearch = cpL

cpResults :: forall g. ChildPath (R.StateP g) (ChildState g)  R.QueryP ChildQuery ListSlot ChildSlot
cpResults = cpR :> cpL

cpDetail :: forall g. ChildPath D.State (ChildState g)  D.Query ChildQuery DetailSlot ChildSlot
cpDetail = cpR :> cpR


type StateP g = ParentState State (ChildState g) Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
type PeekP g = ParentDSL State (ChildState g) Query ChildQuery g ChildSlot Unit

type State = { currentPage :: Routes }

data Routes
    = Home
    | SearchResult
    | DetailResult

instance showRoutes :: Show Routes where
    show Home = "Home"
    show SearchResult = "SearchResult"
    show DetailResult = "DetailResult"

routing :: Match Routes
routing = SearchResult <$ lit "" <* lit "search"
      <|> DetailResult <$ lit "" <* lit "detail"
      <|> Home <$ lit ""

ui :: forall eff. Component (StateP (Affect eff)) QueryP (Affect eff)
ui = parentComponent { render, eval, peek: Just peek }
    where

    render :: State -> ParentHTML (ChildState (Affect eff)) Query ChildQuery (Affect eff) ChildSlot
    render st =
        H.div_
            [

              H.slot' cpSearch SearchSlot \_ -> { component: S.search, initialState: S.initState}
            , H.slot' cpResults ListSlot \_ -> { component: R.results, initialState: (parentState R.initState) }
            , H.slot' cpDetail DetailSlot \_ -> { component: D.detail , initialState: D.initState }
            , H.div_ [ H.text $ show st.currentPage ]
            ]

    eval :: Natural Query (ParentDSL State (ChildState (Affect eff)) Query ChildQuery (Affect eff) ChildSlot)
    eval (Goto p next) = do
        modify (\s -> { currentPage: p })
        pure next

    peek :: forall a. ChildF ChildSlot ChildQuery a -> PeekP (Affect eff)
    peek (ChildF p q) = coproduct (peekSearch p) peekListAndDetail q

    peekSearch :: forall a. ChildSlot -> S.Query a -> PeekP (Affect eff)
    peekSearch (Left p) (S.Submit _) = do
        search <- query' cpSearch p (request S.GetQuery)
        query' cpResults ListSlot $ left (action (R.SetResults $ Just DD.dummyList))
        pure unit
    peekSearch _ _ = pure unit

    peekListAndDetail :: forall a. Coproduct R.QueryP D.Query a -> PeekP (Affect eff)
    peekListAndDetail = coproduct peekResults (const (pure unit))

    peekResults :: forall a. R.QueryP a -> PeekP (Affect eff)
    peekResults = coproduct (const (pure unit)) peekResult

    peekResult :: forall a. ChildF R.ResultSlot Re.Query a -> PeekP (Affect eff)
    peekResult (ChildF (R.ResultSlot p) (Re.Select _)) = do
        query' cpDetail DetailSlot (action (D.Load (Just p)))
        pure unit


type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver QueryP eff -> Aff (Effects eff) Unit
routeSignal driver = do
    Tuple old new <- matchesAff routing
    redirects driver old new

redirects :: forall eff. Driver QueryP eff -> Maybe Routes -> Routes -> Aff (Effects eff) Unit
redirects driver _ SearchResult = do
    driver $ left (action (Goto SearchResult))
redirects _ _ _ = pure unit

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState { currentPage: Home }) body
    routeSignal driver
