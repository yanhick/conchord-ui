module Main where

import Prelude

import Data.Functor ((<$))
import Routing
import Routing.Match
import Routing.Match.Class
import Routing.Hash
import Control.Apply ((<*), (*>))
import Control.Alt ((<|>))
import Control.Monad.Free (liftF)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console (print)
import Data.Int (floor)
import Data.Functor.Coproduct (Coproduct, coproduct, left)
import Data.Generic (class Generic, gEq, gCompare)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Foreign.Class (readJSON)
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), post)
import Data.Tuple (Tuple(Tuple))
import DOM
import Control.Monad.Aff.AVar
import Control.Monad.Eff.Exception

import Halogen (HalogenEffects, ParentDSL, Natural,
               ParentHTML, Component, ParentState, ChildF(ChildF),
               parentState, runUI, parentComponent, modify, request, query', action,
               Driver, HTML, SlotConstructor, fromEff, fromAff
               )
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Search as S
import Results as R
import Result as Re
import Detail as D
import Model as M
import Api as A
import Routing.Match (Match)

type AppEffects eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)
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
    | SearchResult String
    | DetailResult Int

instance eqRoutes :: Eq Routes where
    eq Home Home = true
    eq (SearchResult _) (SearchResult _) = true
    eq (DetailResult _) (DetailResult _) = true
    eq _ _ = false

instance showRoutes :: Show Routes where
    show Home = "/home"
    show (SearchResult q) = "/search/?q=" <> show q
    show (DetailResult i)= "/detail/" <> show i

int :: Match Int
int = floor <$> num

routing :: Match Routes
routing = search
      <|> detail
      <|> Home <$ lit ""

detail :: Match Routes
detail = DetailResult <$> (lit "" *> lit "detail" *> int)

search :: Match Routes
search = SearchResult <$> (lit "" *> lit "search" *> (param "q"))

routedComponent :: forall s i j . (Eq s) => s -> s -> HTML i j  -> HTML i j
routedComponent r r' c | r == r' = H.div_ [c]
routedComponent _ _ c = H.div [ P.class_ $ H.className "hidden" ] [c]

ui :: forall eff. Component (StateP (Affect eff)) QueryP (Affect eff)
ui = parentComponent { render, eval, peek: Just peek }
    where

    render :: State -> ParentHTML (ChildState (Affect eff)) Query ChildQuery (Affect eff) ChildSlot
    render st =
        H.div_
            [
              (routedComponent st.currentPage (SearchResult "") $ H.slot' cpResults ListSlot \_ -> { component: R.results, initialState: (parentState R.initState) })
            , (routedComponent st.currentPage Home $ H.slot' cpSearch SearchSlot \_ -> { component: S.search, initialState: S.initState})
            , (routedComponent st.currentPage (DetailResult 0) $ H.slot' cpDetail DetailSlot \_ -> { component: D.detail , initialState: D.initState })
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
        changePage $ SearchResult $ fromMaybe "" search
        query' cpResults ListSlot $ left (action (R.SetResults Nothing))
        r <- fromAff $ A.fetchResults $ fromMaybe "" search
        query' cpResults ListSlot $ left (action (R.SetResults $ Just $ either (\e -> [M.Result {id: 1, desc: show e, title: "dasf"}]) id $ readJSON r))
        pure unit
    peekSearch _ _ = pure unit

    peekListAndDetail :: forall a. Coproduct R.QueryP D.Query a -> PeekP (Affect eff)
    peekListAndDetail = coproduct peekResults (const (pure unit))

    peekResults :: forall a. R.QueryP a -> PeekP (Affect eff)
    peekResults = coproduct (const (pure unit)) peekResult

    peekResult :: forall a. ChildF R.ResultSlot Re.Query a -> PeekP (Affect eff)
    peekResult (ChildF (R.ResultSlot p) _) = do
        query' cpDetail DetailSlot (action (D.Load (Just p)))
        changePage $ DetailResult p
        pure unit


changePage :: forall eff. Routes -> PeekP (Affect eff)
changePage r = do
    modify _ { currentPage = r }
    fromEff $ modifyHash \_ -> show r

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver QueryP eff -> Aff (Effects eff) Unit
routeSignal driver = do
    Tuple old new <- matchesAff routing
    redirects driver old new

redirects :: forall eff. Driver QueryP eff -> Maybe Routes -> Routes -> Aff (Effects eff) Unit
redirects driver _ (SearchResult q) = do
    driver $ left (action (Goto $ SearchResult q))
redirects driver _ Home = do
    driver $ left (action (Goto Home))
redirects driver _ (DetailResult i) = do
    driver $ left (action (Goto $ DetailResult i))

main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    driver <- runUI ui (parentState { currentPage: Home }) body
    routeSignal driver
