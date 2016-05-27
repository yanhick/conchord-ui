module View where

import Prelude (($), (<$>), show, const)

import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)

import Pux.Html (Html, section, div, p, text, header, article
                , h1, h2, h3, span, b, nav, li, button
                , (#), (!), bind)
import Pux.CSS (style, px, fontSize)
import Pux.Router (link)
import Pux.Html.Events (onClick)

import Model (Song, SongMeta, SongContent, SongSection, SongLyric)
import Action (Action(UIAction), UIAction(Increment, Decrement))

song :: Maybe Song -> Number -> Html Action
song Nothing _ = div # text "No song"
song (Just s) fontSize =
    div # do
        header_
        songMeta s.meta
        songContent s.content fontSize

songMeta :: SongMeta -> Html Action
songMeta s =
    header # do
       h1 # text s.title
       h2 # text s.artist
       maybe (text "") (\a -> h3 # text a) s.album

songContent :: SongContent -> Number -> Html Action
songContent s fs = article [ style $ fontSize (px fs) ] (songSection <$> s)

songSection :: SongSection -> Html Action
songSection s =
    section # do
        h2 # text (show s.name)
        p [] (songLyric <$> s.lyrics)

songLyric :: SongLyric -> Html Action
songLyric l =
    span # do
        b # text (show l.chord)
        text $ fromMaybe "" l.text

header_:: Html Action
header_ =
    header # do
        nav # do
            link "/" [] [ text "Home" ]
            songTextSize

songTextSize :: Html Action
songTextSize =
    li # do
        button ! onClick (const  (UIAction Increment)) # text "+"
        button ! onClick (const  (UIAction Decrement)) # text "-"
