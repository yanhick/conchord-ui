module SongTextSize where

import Prelude (const)

import Pux.Html (Html, li, button, text, (!), (#), bind)
import Pux.Html.Events (onClick)

import Action (Action(UIAction), UIAction(Increment, Decrement))

view :: Html Action
view =
    li # do
        button ! onClick (const  (UIAction Increment)) # text "+"
        button ! onClick (const  (UIAction Decrement)) # text "-"

