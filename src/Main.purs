module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Either
import Data.Maybe
import Data.Functor

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Search as S
import qualified Results as R

type State = { on :: Boolean, label :: String }

data ListSlot = ListSlot

derive instance genericSlot :: Generic ListSlot
instance eqSlot :: Eq ListSlot where eq = gEq
instance ordSlot :: Ord ListSlot where compare = gCompare


initialState :: State
initialState = { on: false, label: "" }

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

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Nothing }
    where

    render :: State -> ParentHTML (ChildState g) Query ChildQuery g ChildSlot
    render st =
        H.div_
            [ H.text "Hello"
            , H.slot' cpSearch S.Slot \_ -> { component: S.search, initialState: S.initState}
            , H.slot' cpResults ListSlot \_ -> { component: R.results, initialState: (parentState R.initState)}
            ]

    eval :: Natural Query (ParentDSL State (ChildState g) Query ChildQuery g ChildSlot)
    eval (ReadStates next) = pure next


main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    runUI ui (parentState initialState) body
