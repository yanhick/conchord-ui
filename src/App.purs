module App where

import Prelude (pure)

import Data.Maybe (Maybe(Nothing))
import Data.Foreign (F)

import Model (SearchResults, Song, song)
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
  , song :: F Song
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
    , song: pure song
  }
}

