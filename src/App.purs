module App where

import Prelude (id)
import Pux.Html (Html, div, text)

data Action = Action
type State = Int

view :: State -> Html Action
view _ = div [] [ text "Result" ]

update :: Action -> State -> State
update _ = id
