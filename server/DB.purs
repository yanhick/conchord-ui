module DB where

import Prelude (bind, ($), pure, (<<<), class Show, (<$>), Unit)

import Control.Monad.Aff (Aff)

import Data.String (fromCharArray)
import Data.Traversable (traverse)
import Data.Array (fromFoldable)
import Data.Int (fromString)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Either (Either())
import Data.Generic (class Generic, gShow)
import Data.Foreign.Class (class IsForeign, readProp)

import Text.Parsing.StringParser (Parser, fail, runParser, ParseError)
import Text.Parsing.StringParser.String (string, eof, anyChar, anyDigit)
import Text.Parsing.StringParser.Combinators (manyTill)

import Database.Postgres (ConnectionInfo(), DB, query, queryOne, queryValue, execute, withConnection, Query(Query))
import Database.Postgres.SqlValue (toSql)

import Model (SongMeta(SongMeta), Year(Year), Song(Song), serializeSong, parseSong, DBSong(DBSong))

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
getSongById c id =
    withConnection c \client -> do
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

getSearchResults :: forall e. ConnectionInfo -> String -> Aff ( db :: DB | e ) (Either ParseError (Array DBSong))
getSearchResults c q =
    withConnection c \client -> do
        rows <- query getSearchResultsQuery [toSql q] client
        pure $ traverse songTableRowToDBSong rows

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

createSong :: forall e. ConnectionInfo -> Song -> Aff ( db :: DB | e ) (Maybe (Either ParseError DBSong))
createSong c s@(Song { meta: SongMeta m@{ year: Year y } }) =
    withConnection c \client -> do
        songTableRow <- queryOne createSongQuery [
          toSql m.title,
          toSql m.artist,
          toSql m.album,
          toSql y,
          toSql $ serializeSong s
        ] client
        pure $ songTableRowToDBSong <$> songTableRow


createSongQuery :: Query SongTableRow
createSongQuery = Query ("""

    INSERT into song
    VALUES(default, $1, $2, $3, $4, $5)
    RETURNING *

""")

deleteSong :: forall e. ConnectionInfo -> Int -> Aff ( db :: DB | e ) Unit
deleteSong c id =
    withConnection c \client -> do
        execute deleteSongQuery [
            toSql id
        ] client

deleteSongQuery :: Query String
deleteSongQuery = Query ("""

    DELETE FROM song
    WHERE id = $1

""")

updateSong :: forall e. ConnectionInfo -> Int -> Song -> Aff ( db :: DB | e ) (Maybe (Either ParseError DBSong))
updateSong c id s@(Song { meta: SongMeta m@{ year: Year y } }) =
    withConnection c \client -> do
        songTableRow <- queryOne updateSongQuery [
          toSql m.title,
          toSql m.artist,
          toSql m.album,
          toSql y,
          toSql $ serializeSong s,
          toSql id
        ] client
        pure $ songTableRowToDBSong <$> songTableRow

updateSongQuery :: Query SongTableRow
updateSongQuery = Query ("""

    UPDATE song
    SET title = $1,
        artist = $2,
        album = $3,
        year = $4,
        content = $5
    WHERE id = $6
    RETURNING *

""")

newtype SongTableRow = SongTableRow {
    id :: String,
    title :: String,
    artist :: String,
    album :: String,
    year :: Int,
    content :: String
}

songTableRowToDBSong :: SongTableRow -> Either ParseError DBSong
songTableRowToDBSong (SongTableRow { id, content }) = do
    song <- runParser parseSong content
    pure $ DBSong { id, song }

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
