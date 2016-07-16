module App where

import Data.Foreign (F)

import Model (SearchResults, Song)
import Route (Route(HomePage))


--- App State

type State = {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

type UIState = {
    searchQuery :: String
}

data AsyncData a = Loaded (F a) | Loading | Empty

type IOState = {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
}

init :: State
init = {
    currentPage: HomePage
  , ui: {
      searchQuery: ""
  }
  , io: {
      searchResults: Empty
    , song: Empty
  }
}

