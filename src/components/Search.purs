module Search where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Data.Functor (($>))
import Control.Monad.Eff.Console
import qualified Halogen.HTML.Events.Handler as EH
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

newtype Search = Search { q :: String }
data Query a = Submit a 
             | Change String a
             | GetQuery (String -> a)

data Slot = Slot Int

derive instance genericSlot :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

initState :: Search
initState = Search { q: "" }

search :: forall g. (Functor g) => Component Search Query g
search = component { render, eval }
    where

    render :: Search -> ComponentHTML Query
    render (Search st) =
        H.form
        [ E.onSubmit \_ -> EH.preventDefault $> action Submit ]
        [ H.input [ P.inputType P.InputText
                  , P.placeholder st.q
                  , E.onValueChange (E.input Change)
                  ]
                  , H.input [ P.inputType P.InputSubmit ]
        ]

    eval :: Natural Query (ComponentDSL Search Query g)
    eval ( Submit next ) = pure next
    eval ( Change desc next ) = do
        modify (\(Search st) -> Search $ st { q = desc })
        pure next
    eval ( GetQuery continue ) = do
        q <- gets (\(Search st) -> st.q)
        pure (continue q)
