module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import DOM (DOM())
import Signal.Channel (CHANNEL())
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX())
import Data.Foreign.Class (readJSON)
import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe(Just, Nothing))

import Pux (renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Route (match)
import App (State, init)
import Action (update, Action(PageView))
import View (view)


main :: String -> Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION,
    console :: CONSOLE
) Unit
main state = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)

    let s = case readJSON state of
              Right state' -> state'
              Left _ -> init

    app <- start {
      initialState: s
    , update: update
    , view: view
    , inputs: [routeSignal]
    }

    renderToDOM "#app" app.html
