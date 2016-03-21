module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Either
import Data.Maybe

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Search as S

type State = { on :: Boolean, label :: String }



initialState :: State
initialState = { on: false, label: "" }

data ToggleSlot = ToggleSlot

derive instance genericToggleSlot :: Generic ToggleSlot
instance eqToggleSlot :: Eq ToggleSlot where eq = gEq
instance ordToggleSlot :: Ord ToggleSlot where compare = gCompare

data Query a = ReadStates a

type ChildState = Either S.State ToggleState
type ChildQuery = Coproduct S.Query ToggleQuery
type ChildSlot = Either S.Slot ToggleSlot

cpToggle :: ChildPath State ChildState ToggleQuery ChildQuery ToggleSlot ChildSlot
cpToggle = cpR

cpSearch :: ChildPath S.State ChildState S.Query ChildQuery S.Slot ChildSlot
cpSearch = cpL


type StateP g = ParentState State ChildState Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Nothing }
    where

    render :: State -> ParentHTML ChildState Query ChildQuery g ChildSlot
    render st =
        H.div_
            [ H.text "Hello"
            , H.slot' cpSearch S.Slot \_ -> { component: S.search, initialState: S.initState}
            ]

    eval :: Natural Query (ParentDSL State ChildState Query ChildQuery g ChildSlot)
    eval (ReadStates next) = pure next

type ToggleState = { on :: Boolean, label :: String }

data ToggleQuery a = Toggle a | ToggleUpdateLabel a


toggleButton :: forall g. (Functor g) => Component ToggleState ToggleQuery g
toggleButton = component { render, eval }
    where
        render :: ToggleState -> ComponentHTML ToggleQuery
        render state =
            H.div_
                [ H.h1
                    [ E.onMouseOver (E.input_ ToggleUpdateLabel) ]
                    [ H.text state.label]
                , H.p_
                    [ H.text "Why not toggle this button:" ]
                , H.button
                    [ E.onClick (E.input_ Toggle) ]
                    [ H.text
                        if not state.on
                        then "Don't push me"
                        else "I said don't push me!"
                    ]
                ]

        eval :: Natural ToggleQuery (ComponentDSL ToggleState ToggleQuery g)
        eval (Toggle next) = do
            modify (\state -> state { on = not state.on })
            pure next
        eval (ToggleUpdateLabel next) = do
            modify (\state -> state { label = state.label <> "!" })
            pure next


main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
    body <- awaitBody
    runUI ui (parentState initialState) body
