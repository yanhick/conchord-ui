module App where

import Prelude (bind, ($), pure, otherwise)

import Data.Foreign (F, isNull, readString, ForeignError(TypeMismatch))
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Either (Either(Left, Right))
import Data.Generic (class Generic, gEq, gShow)
import Data.Foreign.Generic (readGeneric, defaultOptions, toJSONGeneric)

import Model (SearchResults, SearchResult, Song)
import Route (Route(HomePage))


--- App State

newtype State = State {
    currentPage :: Route
  , ui :: UIState
  , io :: IOState
}

derive instance genericState :: Generic State

instance isForeignState :: IsForeign State where
    read = readGeneric defaultOptions { unwrapNewtypes = true }


data HeaderVisibility = ShowHeader | HideHeader | PendingHideHeader

derive instance genericHeaderVisibility :: Generic HeaderVisibility

instance isForeignHeaderVisibility :: IsForeign HeaderVisibility where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

toHeaderVisibility :: String -> F HeaderVisibility
toHeaderVisibility "ShowHeader" = pure ShowHeader
toHeaderVisibility "HideHeader" = pure HideHeader
toHeaderVisibility "PendingHideHeader" = pure PendingHideHeader
toHeaderVisibility s = Left $ TypeMismatch "HeaderVisibility" s

newtype UIState = UIState {
    searchQuery :: String,
    headerVisibility :: HeaderVisibility
}

derive instance genericUIState :: Generic UIState


instance isForeignUIState :: IsForeign UIState where
    read = readGeneric defaultOptions { unwrapNewtypes = true }

type Error = String
data AsyncData a = Loaded a | Loading | Empty | LoadError String

derive instance genericAsyncDataSearchResult :: Generic (AsyncData (Array SearchResult))
derive instance genericAsyncDataSong :: Generic (AsyncData Song)


instance isForeignAsyncDataSong :: IsForeign (AsyncData Song) where
    read = readGeneric defaultOptions { unwrapNewtypes = true }


newtype IOState = IOState {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
}

derive instance genericIOState :: Generic IOState

instance isForeignIOState :: IsForeign IOState where
    read = readGeneric defaultOptions { unwrapNewtypes = true }


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
  }
}


