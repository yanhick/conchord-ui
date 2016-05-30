module App where

import Data.Maybe (Maybe(Nothing))

import Model (SearchResults, Song)
import Route (Route(HomePage))


--- App State

type State = {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

type UIState = {
    songFontSize :: Number
  , searchQuery :: String
}

type IOState = {
    searchResults :: SearchResults
  , song :: Maybe Song
}

init :: State
init = {
    currentPage: HomePage
  , ui: {
      songFontSize: 12.0
    , searchQuery: ""
  }
  , io: {
      searchResults: []
    , song: Nothing
  }
}

