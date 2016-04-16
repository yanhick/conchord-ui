module DummyData where

import Model as M
import Data.Foreign.Class (readJSON)
import Data.Foreign (F)

dummyListJson :: String
dummyListJson = "[{\"id\": 0, \"title\": \"first\", \"desc\": \"this is the first\"}]"

getDummyList :: F M.List
getDummyList = readJSON dummyListJson
