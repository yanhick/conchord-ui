module Song where

import Data.Maybe (Maybe(Nothing, Just))
import Pux.Html (Html, div, text, (#), bind)
import Model (Song)
import Action (Action())
import SongMeta as Sm
import SongContent as Sc
import Header as H

view :: Maybe Song -> Html Action
view Nothing = div # text "No song"
view (Just s) =
    div # do
        H.view
        Sm.view s.meta
        Sc.view s.content
