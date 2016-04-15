module DummyData where

import Model as M

dummyList :: M.List
dummyList = [
               M.Result {id: 0, title: "first", desc: "this is the first"}
             , M.Result {id: 1, title: "second", desc: "this is the second"}
            ]

dummyListJson :: String
dummyListJson = "[{id: 0, title: \"first\", desc: \"this is the first\"}]"
