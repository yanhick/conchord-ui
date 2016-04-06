module Model where

type Result = { id :: Int, title :: String, desc :: String }

type List = Array Result

type Search = { q :: String }

type Detail = Result
