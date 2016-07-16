module Route where

import Prelude (($), (<$>), (<>), show, bind)
import Data.Functor ((<$))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (stripPrefix)
import Data.Either (Either(Left, Right))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Foreign.Class (class IsForeign)
import Data.Foreign (readString, ForeignError(TypeMismatch), F)

import Data.Argonaut (class EncodeJson, (:=), (~>), fromString)

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
toRoute r =
    case stripPrefix "SongPage " r of
        Just id -> Right $ SongPage 0
        Nothing ->
            case stripPrefix "SearchResultPage " r of
                Just q -> Right $ SearchResultPage q
                Nothing -> Left $ TypeMismatch "Route" r


match :: String -> Route
match url = fromMaybe NotFoundPage $ router url $
            HomePage <$ end
            <|>
            SongPage <$> (lit "song" *> int) <* end
            <|>
            SearchResultPage <$> (lit "search" *> (param "q")) <* end
