module SongLyric where

import Prelude (($))

import Data.Maybe (fromMaybe)
import Pux.Html (Html, span, b, text, (#), bind)

import Action (Action())
import Model (SongLyric)

view :: SongLyric -> Html Action
view l =
    span # do
        b # text "chord"
        text $ fromMaybe "" l.text


