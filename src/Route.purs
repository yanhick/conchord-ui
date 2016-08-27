module Route where

import Prelude (($), (<$>), (<>), class Eq, (==))
import Global (decodeURIComponent)

import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Data.Generic (class Generic, gEq)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric, defaultOptions)


import Pux.Router (router, lit, int, end, param)


data Route = HomePage | SearchResultPage String | SongPage Int | NewSongPage | UpdateSongPage Int | NotFoundPage | SongPageZen Int

derive instance genericRoute :: Generic Route

instance isForeignRoute :: IsForeign Route where
    read = readGeneric defaultOptions

instance eqRoute :: Eq Route where
    eq = gEq

match :: String -> Route
match url = fromMaybe NotFoundPage $ router url $
            HomePage <$ end
            <|>
            SongPage <$> (lit "song" *> int) <* end
            <|>
            SongPageZen <$> (lit "song-zen" *> int) <* end
            <|>
            NewSongPage <$ (lit "new") <* end
            <|>
            UpdateSongPage <$> (lit "update" *> int) <* end
            <|>
            SearchResultPage <$> (lit "search" *> (decodeURIComponent <$> (param "q"))) <* end
