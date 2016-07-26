module DB where

import Prelude (bind, ($), pure, (<<<), const)

import Data.String (fromCharArray)
import Data.Array (fromFoldable)
import Data.Int (fromString)
import Data.Either (Either(Left, Right))
import Data.Maybe (maybe, Maybe(Nothing, Just))

import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.String (string, eof, anyChar, anyDigit)
import Text.Parsing.StringParser.Combinators (manyTill, lookAhead)

import Database.Postgres (ConnectionInfo())

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
