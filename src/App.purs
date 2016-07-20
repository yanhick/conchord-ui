module App where

import Prelude (bind, ($), pure, otherwise)

import Data.Foreign (F, isNull, readString, ForeignError(TypeMismatch))
import Data.Foreign.Class (class IsForeign, readProp, read)
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

instance isForeignState :: IsForeign State where
    read value = do
        currentPage <- readProp "currentPage" value
        ui <- readProp "ui" value
        io <- readProp "io" value
        pure $ State { currentPage, ui, io }


instance encodeJsonState :: EncodeJson State where
    encodeJson (State { currentPage, ui, io })
        = "currentPage" := encodeJson currentPage
        ~> "ui" := encodeJson ui
        ~> "io" := encodeJson io
        ~> jsonEmptyObject

data HeaderVisibility = ShowHeader | HideHeader | PendingHideHeader

instance isForeignHeaderVisibility :: IsForeign HeaderVisibility where
    read value = do
        s <- readString value
        toHeaderVisibility s

toHeaderVisibility :: String -> F HeaderVisibility
toHeaderVisibility "ShowHeader" = pure ShowHeader
toHeaderVisibility "HideHeader" = pure HideHeader
toHeaderVisibility "PendingHideHeader" = pure PendingHideHeader
toHeaderVisibility s = Left $ TypeMismatch "HeaderVisibility" s

newtype UIState = UIState {
    searchQuery :: String,
    headerVisibility :: HeaderVisibility
}

instance encodeJsonUIState :: EncodeJson UIState where
    encodeJson (UIState { searchQuery })
        = "searchQuery" := searchQuery
        ~> jsonEmptyObject

instance isForeignUIState :: IsForeign UIState where
    read value = do
        searchQuery <- readProp "searchQuery" value
        headerVisibility <- readProp "headerVisibility" value
        pure $ UIState { searchQuery, headerVisibility }

data AsyncData a = Loaded (F a) | Loading | Empty

instance encodeJsonAsyncData :: (EncodeJson d) => EncodeJson (AsyncData d) where
    encodeJson (Loaded (Right d)) = encodeJson d
    encodeJson (Loaded (Left _)) = jsonNull
    encodeJson Loading = jsonNull
    encodeJson Empty = jsonNull

instance isForeignAsyncData :: (IsForeign a) => IsForeign (AsyncData a) where
    read value | isNull value = pure Empty
               | otherwise = pure (Loaded (read value))


newtype IOState = IOState {
    searchResults :: AsyncData SearchResults
  , song :: AsyncData Song
}

instance isForeignIOState :: IsForeign IOState where
    read value = do
        searchResults <- readProp "searchResults" value
        song <- readProp "song" value
        pure $ IOState { searchResults, song }

instance encodeJsonIOState :: EncodeJson IOState where
    encodeJson (IOState { searchResults, song })
        = "searchResults" := encodeJson searchResults
        ~> "song" := encodeJson song
        ~> jsonEmptyObject

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


