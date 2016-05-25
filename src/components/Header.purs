module Header where

import Pux.Html (Html, header, nav, text, (#), bind)
import Pux.Router (link)

import Action (Action())

view :: Html Action
view =
    header # do
        nav # link "/" [] [ text "Home" ]
