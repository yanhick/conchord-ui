module SongLyric where

import Prelude (($), show)

import Data.Maybe (fromMaybe)
import Pux.Html (Html, span, b, text, (#), bind)

import Action (Action())
import Model (SongLyric)

view :: SongLyric -> Html Action
view l =
    span # do
        b # text (show l.chord)
        text $ fromMaybe "" l.text


