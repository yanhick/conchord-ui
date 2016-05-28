module Route where

import Prelude (($), (<$>))
import Data.Functor ((<$))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, int, end, param)


data Route = HomePage | SearchResultPage String | SongPage Int | NotFoundPage

match :: String -> Route
match url = fromMaybe NotFoundPage $ router url $
            HomePage <$ end
            <|>
            SongPage <$> (lit "detail" *> int) <* end
            <|>
            SearchResultPage <$> (lit "search" *> (param "q")) <* end
