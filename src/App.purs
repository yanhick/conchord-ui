module App where

import Data.Foreign (F)
import Data.Argonaut (class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, fromArray)

import Model (SearchResults, Song)
import Route (Route(HomePage))


--- App State

newtype State = State {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

instance encodeJsonState :: EncodeJson State where
    encodeJson (State { currentPage, ui, io })
        = jsonEmptyObject

newtype UIState = UIState {
    searchQuery :: String
}

data AsyncData a = Loaded (F a) | Loading | Empty

newtype IOState = IOState {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
}

init :: State
init = State {
    currentPage: HomePage
  , ui: UIState {
      searchQuery: ""
  }
  , io: IOState {
      searchResults: Empty
    , song: Empty
  }
}

