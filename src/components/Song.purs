module Song where

import Data.Maybe (Maybe(Nothing, Just))
import Pux.Html (Html, div, text, (#), bind)
import Model (Song)
import Action (Action())
import SongMeta as Sm
import SongContent as Sc

view :: Maybe Song -> Html Action
view Nothing = div # text "No song"
view (Just s) =
    div # do
        Sm.view s.meta
        Sc.view s.content
