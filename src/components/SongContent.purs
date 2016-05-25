module SongContent where

import Prelude ((<$>), ($))
import Pux.Html (Html, article)
import Pux.CSS (style, px, fontSize)

import Action (Action())
import Model (SongContent)

import SongSection as Ss

view :: SongContent -> Number -> Html Action
view s fs = article [ style $ fontSize (px fs) ] (Ss.view <$> s)
