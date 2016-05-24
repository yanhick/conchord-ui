module SongMeta where

import Data.Maybe (maybe)
import Pux.Html (Html, div, header, h1, h2, h3, text, (#), bind)

import Action (Action())
import Model (SongMeta)

view :: SongMeta -> Html Action
view s =
    header # do
       h1 # text s.title
       h2 # text s.artist
       maybe (div [] []) (\a -> h3 # text a) s.album
