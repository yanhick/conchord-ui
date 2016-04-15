module Model where

import Data.Foreign.Class (class IsForeign, readProp)
import Prelude

newtype Result = Result { id :: Int, title :: String, desc :: String }

instance resultIsForeign :: IsForeign Result where
    read value = do
        id <- readProp "id" value
        title <- readProp "title" value
        desc <- readProp "desc" value
        return $ Result { id, title, desc }

type List = Array Result

type Search = { q :: String }

type Detail = Result
