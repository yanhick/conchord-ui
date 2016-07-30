module DB where

import Prelude (bind, ($), pure, (<<<), const, class Show, (<$>), Unit)

import Control.Monad.Aff (Aff)

import Data.String (fromCharArray)
import Data.Array (fromFoldable)
import Data.Int (fromString)
import Data.Either (Either(Left, Right))
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.Generic (class Generic, gShow)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Generic (defaultOptions, readGeneric)

import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.String (string, eof, anyChar, anyDigit)
import Text.Parsing.StringParser.Combinators (manyTill, lookAhead)

import Database.Postgres (ConnectionInfo(), DB, connect, query, queryValue, execute, Query(Query))
import Database.Postgres.SqlValue (toSql)

import Model (SearchResult(SearchResult), SongMeta(SongMeta), Year(Year), Song(Song), serializeSong)

localConnectionInfo :: ConnectionInfo
localConnectionInfo = {
    host: "localhost",
    db: "test",
    port: 5432,
    user: "testuser",
    password: "test"
}

mkConnection :: Parser ConnectionInfo
mkConnection = do
    string "postgres://"
    user <- manyTill anyChar (string ":")
    password <- manyTill anyChar (string "@")
    host <- manyTill anyChar (string ":")
    port <- manyTill anyDigit (string "/")
    db <- manyTill anyChar eof
    port' <- case charsToInt port of
                  Nothing -> fail "port is not a valid integer"
                  Just p -> pure p
    pure {
        host: charsToString host,
        db: charsToString db,
        port: port',
        user: charsToString user,
        password: charsToString password
    }
    where charsToString = fromCharArray <<< fromFoldable
          charsToInt = fromString <<< charsToString

getSongById :: forall e. ConnectionInfo -> Int -> Aff ( db :: DB | e ) String
getSongById c id = do
    client <- connect c
    content <- queryValue getSongByIdQuery [toSql id] client
    pure $ case content of
          Just s -> s
          Nothing -> ""

getSongByIdQuery :: Query String
getSongByIdQuery = Query ("""

    SELECT content
    FROM song
    WHERE id = $1

""")

getSearchResults :: forall e. ConnectionInfo -> String -> Aff ( db :: DB | e ) (Array SearchResult)
getSearchResults c q = do
    client <- connect c
    rows <- query getSearchResultsQuery [toSql q] client
    pure $ rowToSearchResult <$> rows
      where
        rowToSearchResult (SongTableRow { id, title, artist, album, year, content }) = SearchResult { id: 1, meta: SongMeta { artist, album, year: Year year, title }, desc: content }

getSearchResultsQuery :: Query SongTableRow
getSearchResultsQuery = Query ("""

    SELECT id, title, artist, album, year, content
    FROM
    (SELECT song.id as id,
            song.title as title,
            song.artist as artist,
            song.album as album,
            song.year as year,
            song.content as content,
            to_tsvector(song.content) as document
            FROM song) as doc
    WHERE document @@ to_tsquery($1)

""")

createSong :: forall e. ConnectionInfo -> Song -> Aff ( db :: DB | e ) Unit
createSong c s@(Song { meta: SongMeta m@{ year: Year y } }) = do
    client <- connect c
    execute createSongQuery [
      toSql m.title,
      toSql m.artist,
      toSql m.album,
      toSql y,
      toSql $ serializeSong s
    ] client

createSongQuery :: Query String
createSongQuery = Query ("""

    INSERT into song
    VALUES(default, $1, $2, $3, $4, $5)

""")

deleteSong :: forall e. ConnectionInfo -> Int -> Aff ( db :: DB | e ) Unit
deleteSong c id = do
    client <- connect c
    execute deleteSongQuery [
        toSql id
    ] client

deleteSongQuery :: Query String
deleteSongQuery = Query ("""

    DELETE FROM song
    WHERE id = $1

""")

updateSong :: forall e. ConnectionInfo -> Int -> Song -> Aff ( db :: DB | e ) Unit
updateSong c id s@(Song { meta: SongMeta m@{ year: Year y } }) = do
    client <- connect c
    execute updateSongQuery [
      toSql m.title,
      toSql m.artist,
      toSql m.album,
      toSql y,
      toSql $ serializeSong s,
      toSql id
    ] client

updateSongQuery :: Query String
updateSongQuery = Query ("""

    UPDATE song
    SET title = $1,
        artist = $2,
        album = $3,
        year = $4,
        content = $5
    WHERE id = $6

""")

newtype SongTableRow = SongTableRow {
    id :: String,
    title :: String,
    artist :: String,
    album :: String,
    year :: Int,
    content :: String
}

--- Generic boilerplate

derive instance genericSongTableRow :: Generic SongTableRow

instance isForeignSongTableRow :: IsForeign SongTableRow where
    read value = do
        id <- readProp "id" value
        title <- readProp "title" value
        artist <- readProp "artist" value
        album <- readProp "album" value
        year <- readProp "year" value
        content <- readProp "content" value
        pure $ SongTableRow { id, title, artist, album, year, content }

instance showSongTableRow :: Show SongTableRow where
    show = gShow
