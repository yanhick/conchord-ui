module SongSection where

import Prelude ((<$>))

import Pux.Html (Html, section)

import Action (Action())
import Model (SongSection)

import SongLyric as Sl

view :: SongSection -> Html Action
view s = section [] (Sl.view <$> s.lyrics)

