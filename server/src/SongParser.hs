module SongParser where

import qualified Data.Text as T
import SongSectionParser
import SongChordsParser

data Song = SongMeta (Maybe [Chord]) [SongSection] deriving Show
type SongParserError = String

getSongMetaLines :: [T.Text] -> [T.Text] -> ([T.Text], [T.Text])
getSongMetaLines [] ls = (ls, [])
getSongMetaLines (x:xs) ls
  | T.take 1 x == T.pack "-" = (ls, xs)
  | otherwise = getSongMetaLines xs (x:ls)

parseSong :: T.Text -> Either SongParserError String
parseSong t = do
  songMeta <- getSongMetaLines (T.lines t) []
  return (Left "" "")
