module Main where

import Prelude

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, div, text)
import Pux.Router (sampleUrl)
import Signal ((~>))


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


