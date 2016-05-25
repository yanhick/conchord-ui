module Action where

import Model as M
import Pux.Html.Events (FormEvent)
import Data.Foreign (F)
import Route as R

data Action =
    SearchChange FormEvent |
    RequestDetail Int |
    ReceiveDetail (F M.Detail) |
    RequestSearch |
    ReceiveSearch (F M.List) |
    UIAction UIAction |
    PageView R.Route

data UIAction =
    Increment |
    Decrement
