module Route where

import Prelude (($), (<$>), (<>), show, bind)
import Data.Functor ((<$))
import Data.Either (Either(Left, Right))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Foreign (F)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign (readString, ForeignError(TypeMismatch))

import Data.Argonaut (class EncodeJson, (:=), (~>), fromString)

import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, int, end, param)


data Route = HomePage | SearchResultPage String | SongPage Int | NotFoundPage

instance encodeJsonRoute :: EncodeJson Route where
    encodeJson HomePage = fromString "HomePage"
    encodeJson (SearchResultPage q) = fromString $ "SearchResultPage " <> q
    encodeJson (SongPage id) = fromString $ "SongPage " <> show id
    encodeJson NotFoundPage = fromString "NotFoundPage"

instance isForeignRoute :: IsForeign Route where
    read value = do
        s <- readString value
        toRoute s

toRoute :: String -> F Route
toRoute "HomePage" = Right HomePage
toRoute "NotFoundPage" = Right NotFoundPage
toRoute _ = Left $ TypeMismatch "Route" "Not Route"


match :: String -> Route
match url = fromMaybe NotFoundPage $ router url $
            HomePage <$ end
            <|>
            SongPage <$> (lit "song" *> int) <* end
            <|>
            SearchResultPage <$> (lit "search" *> (param "q")) <* end
