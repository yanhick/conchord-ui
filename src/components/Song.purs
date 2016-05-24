module Song where

import Data.Maybe (Maybe(Nothing, Just))
import Pux.Html (Html, div, text)
import Model (Song)
import Action (Action())
import SongMeta as Sm

view :: Maybe Song -> Html Action
view Nothing = div [] [ text "No song" ]
view (Just s) =
    div []
        [ Sm.view s.meta ]
