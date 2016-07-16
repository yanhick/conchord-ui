module App where

import Data.Foreign (F)
import Data.Either (Either(Left, Right))
import Data.Argonaut (class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject, jsonNull)

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
        = "currentPage" := encodeJson currentPage
        ~> "ui" := encodeJson ui
        ~> "io" := encodeJson io
        ~> jsonEmptyObject

newtype UIState = UIState {
    searchQuery :: String
}

instance encodeJsonUIState :: EncodeJson UIState where
    encodeJson (UIState { searchQuery })
        = "searchQuery" := searchQuery
        ~> jsonEmptyObject

data AsyncData a = Loaded (F a) | Loading | Empty

instance encodeJsonAsyncData :: (EncodeJson d) => EncodeJson (AsyncData d) where
    encodeJson (Loaded (Right d)) = encodeJson d
    encodeJson (Loaded (Left _)) = jsonNull
    encodeJson Loading = jsonNull
    encodeJson Empty = jsonNull

newtype IOState = IOState {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
}

instance encodeJsonIOState :: EncodeJson IOState where
    encodeJson (IOState { searchResults, song })
        = "searchResults" := encodeJson searchResults
        ~> "song" := encodeJson song
        ~> jsonEmptyObject

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

