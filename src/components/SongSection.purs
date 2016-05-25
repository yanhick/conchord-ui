module SongSection where

import Prelude ((<$>), show)

import Pux.Html (Html, section, h2, p, text, (#), bind)

import Action (Action())
import Model (SongSection)

import SongLyric as Sl

view :: SongSection -> Html Action
view s =
    section # do
        h2 # text (show s.name)
        p [] (Sl.view <$> s.lyrics)

