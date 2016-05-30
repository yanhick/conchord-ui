module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import DOM (DOM())
import Signal.Channel (CHANNEL())
import Network.HTTP.Affjax (AJAX())

import Pux (renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Action (Action(PageView))
import Route (match)
import App (init)
import Action (update)
import View (view)


main :: Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION
) Unit
main = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)

    app <- start {
      initialState: init
    , update: update
    , view: view
    , inputs: [routeSignal]
    }
    renderToDOM "#app" app.html


