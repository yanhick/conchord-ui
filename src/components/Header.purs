module Header where

import Pux.Html (Html, header, nav, text, (#), bind)
import Pux.Router (link)

import Action (Action())
import SongTextSize as Sts

view :: Html Action
view =
    header # do
        nav # do
            link "/" [] [ text "Home" ]
            Sts.view
