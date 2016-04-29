module Detail where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (readProp, readJSON, class IsForeign)
import Data.Foreign (F)
import Data.Foreign.Null (runNull)
import Data.Either (Either (Left, Right), either)
import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component, component, modify, fromAff, liftH)
import Halogen.HTML.Indexed as H
import Control.Monad.Free (liftF)
import Results as R
import Api as A
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Console (print)
import Data.Maybe (Maybe (Nothing, Just))

import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))

newtype State = State { id :: Maybe Int, title :: String, desc :: String }

instance detailStateIsForeign :: IsForeign State where
    read value = do
        id <- runNull <$> readProp "id" value
        title <- readProp "title" value
        desc <- readProp "desc" value
        return $ State { id, title, desc }

data Query a = Query a | Load (Maybe Int) a

type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

initState :: State
initState = State { title: "", desc: "default", id: Nothing }

detail :: forall eff. Component State Query (Aff (AppEffects eff))
detail = component { render, eval }
    where

        render :: State -> ComponentHTML Query
        render (State d) =
            H.div_ [ H.h2_ [H.text ("Selected:" <> show d.id)]
                    , H.h3_ [H.text ("fetched: " <> d.desc)]
                    ]

        eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
        eval (Query next) = pure next
        eval (Load id@(Just did) next) = do
            modify (\(State d) -> State $ d { id = id })
            result <- fromAff $ A.fetchDetails did
            let r = parseResult result
            modify (\(State d) -> r)
            pure next
        eval (Load _ next) = pure next

parseResult :: String -> State
parseResult s = either (\d -> State $ { desc: show d, title: "", id: Nothing }) id ((readJSON s) :: F State)
