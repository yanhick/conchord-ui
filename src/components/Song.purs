module Song where

import Data.Maybe (Maybe(Nothing, Just))
import Pux.Html (Html, div, text, (#), bind)
import Model (Song)
import Action (Action())
import SongMeta as Sm
import SongContent as Sc
import Header as H

view :: Maybe Song -> Number -> Html Action
view Nothing _ = div # text "No song"
view (Just s) fontSize =
    div # do
        H.view
        Sm.view s.meta
        Sc.view s.content fontSize
