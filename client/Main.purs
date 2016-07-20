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
import Pux.Router (sampleUrl)
import Signal ((~>))
import Route (match)
import App (init)
import Action (update, Action(PageView))
import View (view)

foreign import getPuxLastState :: forall a. a

main :: Eff (
    dom :: DOM,
    channel :: CHANNEL,
    ajax :: AJAX,
    err :: EXCEPTION,
    console :: CONSOLE
) Unit
main = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)

    state <- getPuxLastState

    app <- start {
      initialState: case readJSON state of
                      Left (_) -> init
                      Right s -> s
    , update: update
    , view: view
    , inputs: [routeSignal]
    }

    renderToDOM "#app" app.html
