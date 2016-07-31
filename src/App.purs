module App where

import Prelude (class Eq, class Show)

import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Either (Either(Right))
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions)

import Model (SearchResults, SearchResult, Song, serializeSong, exampleSong)
import Route (Route(HomePage))

import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Generic (gArbitrary)


--- App State

newtype State = State {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

data HeaderVisibility = ShowHeader | HideHeader | PendingHideHeader

newtype UIState = UIState {
    searchQuery :: String,
    headerVisibility :: HeaderVisibility
}

type Error = String
data AsyncData a = Loaded a | Loading | Empty | LoadError Error

type ValidatedSong = Tuple String (Either Error Song)

newtype IOState = IOState {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
  , newSong :: ValidatedSong
}

init :: State
init = State {
    currentPage: HomePage
  , ui: UIState {
      searchQuery: "",
      headerVisibility: ShowHeader
  }
  , io: IOState {
      searchResults: Empty
    , song: Empty
    , newSong: Tuple (serializeSong exampleSong) (Right exampleSong)
  }
}

--- Generic boilerplate

derive instance genericState :: Generic State

instance isForeignState :: IsForeign State where
    read = readGeneric defaultOptions

instance arbitraryState :: Arbitrary State where
    arbitrary = gArbitrary

instance eqState :: Eq State where
    eq = gEq

instance showState :: Show State where
    show = gShow

derive instance genericHeaderVisibility :: Generic HeaderVisibility

instance isForeignHeaderVisibility :: IsForeign HeaderVisibility where
    read = readGeneric defaultOptions

derive instance genericUIState :: Generic UIState

instance isForeignUIState :: IsForeign UIState where
    read = readGeneric defaultOptions

derive instance genericAsyncDataSearchResult :: Generic (AsyncData (Array SearchResult))
derive instance genericAsyncDataSong :: Generic (AsyncData Song)

instance isForeignAsyncDataSong :: IsForeign (AsyncData Song) where
    read = readGeneric defaultOptions

derive instance genericIOState :: Generic IOState

instance isForeignIOState :: IsForeign IOState where
    read = readGeneric defaultOptions
