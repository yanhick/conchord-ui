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
    songFontSize :: Number
  , searchQuery :: String
}

data SongState = Loaded (F Song) | Loading | Empty

type IOState = {
    searchResults :: SearchResults
  , song :: SongState
}

init :: State
init = {
    currentPage: HomePage
  , ui: {
      songFontSize: 1.0
    , searchQuery: ""
  }
  , io: {
      searchResults: []
    , song: Empty
  }
}
