module SongContent where

import Prelude ((<$>))
import Pux.Html (Html, article)

import Action (Action())
import Model (SongContent)

import SongSection as Ss

view :: SongContent -> Html Action
view s = article [] (Ss.view <$> s)
