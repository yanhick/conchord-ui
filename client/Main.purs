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

import Pux (renderToDOM, start)
import Pux.Devtool (start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))
import Route (match)
import App (State)
import Action (update, Action(PageView))
import View (view)


main :: State -> Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION,
    console :: CONSOLE
) Unit
main state = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)

    app <- start {
      initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal]
    }

    renderToDOM "#app" app.html

debug :: State -> Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION,
    console :: CONSOLE
) Unit
debug state = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)

    app <- Pux.Devtool.start {
      initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal]
    }

    renderToDOM "#app" app.html
