module Route where

import Prelude (($), (<$>), (<>), show)
import Data.Functor ((<$))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Argonaut (class EncodeJson, (:=), (~>), fromString)

import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, int, end, param)


data Route = HomePage | SearchResultPage String | SongPage Int | NotFoundPage

instance encodeJsonRoute :: EncodeJson Route where
    encodeJson HomePage = fromString "HomePage"
    encodeJson (SearchResultPage q) = fromString $ "SearchResultPage " <> q
    encodeJson (SongPage id) = fromString $ "SongPage " <> show id
    encodeJson NotFoundPage = fromString "NotFoundPage"

match :: String -> Route
match url = fromMaybe NotFoundPage $ router url $
            HomePage <$ end
            <|>
            SongPage <$> (lit "song" *> int) <* end
            <|>
            SearchResultPage <$> (lit "search" *> (param "q")) <* end
