module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

type State = { on :: Boolean, label :: String }

initialState :: State
initialState = { on: false, label: "" }

newtype ToggleSlot = ToggleSlot String

derive instance genericToggleSlot :: Generic ToggleSlot
instance eqToggleSlot :: Eq ToggleSlot where eq = gEq
instance ordToggleSlot :: Ord ToggleSlot where compare = gCompare

data Query a = ReadToggle a


type StateP g = InstalledState State ToggleState Query ToggleQuery g ToggleSlot
type QueryP = Coproduct Query (ChildF ToggleSlot ToggleQuery)

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent' render eval (const (pure unit))
    where

    render :: State -> ParentHTML ToggleState Query ToggleQuery g ToggleSlot
    render st =
        H.div_
            [ H.text "Hello" ]

    eval :: Natural Query (ParentDSL State ToggleState Query ToggleQuery g ToggleSlot)
    eval (ReadToggle next) = pure next

type ToggleState = { on :: Boolean, label :: String }

data ToggleQuery a = Toggle a | ToggleUpdateLabel a


toggleButton :: forall g. (Functor g) => Component ToggleState ToggleQuery g
toggleButton = component render eval
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
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui (installedState initialState)
    onLoad $ appendToBody app.node
