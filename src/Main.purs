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


main :: Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION
) Unit
main = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (App.PageView <<< App.match)

    app <- start {
      initialState: App.init
    , update: App.update
    , view: App.view
    , inputs: [routeSignal]
    }
    renderToDOM "#app" app.html


