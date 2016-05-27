module View where

import Prelude (($), (<$>), show, const)

import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)

import Pux.Html (Html, section, div, p, text, header, article
                , h1, h2, h3, span, b, nav, li, button, ul, form
                , input, (#), (!), bind)
import Pux.CSS (style, px, fontSize)
import Pux.Router (link)
import Pux.Html.Events (onClick, onSubmit, onChange)
import Pux.Html.Attributes (type_, value)

import Model (State, Song, SongMeta, SongContent, SongSection, SongLyric, Result(Result))
import Action (Action(UIAction, IOAction), IOAction(RequestSearch, RequestSong), UIAction(Increment, Decrement, SearchChange))
import Route (Route (Detail, SearchResult, Home, NotFound))


view :: State -> Html Action
view state = div [] [ page state.currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page (Detail _) state = song state.io.song state.ui.songFontSize
page (SearchResult _) state = search state
page Home state = home state
page NotFound _ = notFound

--- NotFound view

notFound :: Html Action
notFound = div [] [ text "not found" ]

--- Search Views

home :: State -> Html Action
home state =
    form ! onSubmit (const $ IOAction RequestSearch) # do
        input [ type_ "text", value state.ui.searchQuery, onChange (\f -> UIAction (SearchChange f))] []
        button [ type_ "submit" ] [ text "search" ]

search :: State -> Html Action
search state = ul [] (searchResult <$> state.io.searchResults)

searchResult :: Result -> Html Action
searchResult (Result {title, id}) =
    li # do
        text title
        button [ onClick (const $ IOAction $ RequestSong id) ] []

--- Song Views

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
