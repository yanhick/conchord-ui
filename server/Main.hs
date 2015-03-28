{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Aeson (ToJSON)
import GHC.Generics

data SearchResult = SearchResult {
  id :: Int
, title :: String
, href :: String
} deriving (Show, Generic)

instance ToJSON SearchResult

main :: IO ()
main = scotty 3000 $ do

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

