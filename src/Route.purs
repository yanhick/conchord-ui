module Route where

import Prelude (($), (<$>))
import Data.Functor ((<$))
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, int, end, param)


data Route = Home | SearchResult String | Detail Int | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
            Home <$ end
            <|>
            Detail <$> (lit "detail" *> int) <* end
            <|>
            SearchResult <$> (lit "search" *> (param "q")) <* end
