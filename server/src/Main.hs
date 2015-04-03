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
, artist :: String
, album :: String
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
      json $ [makeSearchResult x | x <- [1..10::Int]]

    get "/api/songs/:id" $ do
      setHeader "Content-Type" "application/json"
      file "server/song.json"

makeSearchResult :: Int -> SearchResult
makeSearchResult resultId = SearchResult resultId "Tokyo vampires and wolves" "/my-song" "The Wombats" "This modern glitch"
