{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Aeson (ToJSON)
import GHC.Generics
import Control.Monad
import System.Environment

data SearchResult = SearchResult {
  id :: Int
, title :: String
, href :: String
} deriving (Show, Generic)

instance ToJSON SearchResult

main :: IO ()
main = do

  port <- liftM read $ getEnv "PORT"
  scotty port $ do

    get "/" $ file "dist/index.html"
    get "/search" $ file "dist/index.html"
    get "/song" $ file "dist/index.html"

    middleware $ staticPolicy (noDots >-> addBase "dist")

    get "/api/search" $
      json $ [
        SearchResult 1 "my-song" "/my-song",
        SearchResult 2 "mo other song" "my-other-song"
      ]

    get "/api/songs/:id" $ do
      setHeader "Content-Type" "application/json"
      file "server/song.json"

