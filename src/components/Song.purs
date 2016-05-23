module Song where

import Data.Maybe (Maybe())
import Model (Song)
import Pux.Html (Html)
import Action (Action())
import Pux.Html (div, text)

view :: Maybe Song -> Html Action
view _ = div [] [ text "Song" ]
