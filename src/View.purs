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

import Model (State, Song, SongMeta, SongContent, SongSection, SongLyric, SearchResult(SearchResult))
import Action (Action(UIAction, IOAction), IOAction(RequestSearch, RequestSong), UIAction(Increment, Decrement, SearchChange))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage))


view :: State -> Html Action
view state = div [] [ page state.currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page (SongPage _) state = songPage state.io.song state.ui.songFontSize
page (SearchResultPage _) state = searchResultPage state
page HomePage state = homePage state
page NotFoundPage _ = notFoundPage

--- Common

type ToolBar = Html Action

header_:: (Maybe ToolBar) -> Html Action
header_ children =
    header # do
        nav # do
            link "/" #
                text "Home"
            fromMaybe (text "") children

--- NotFound view

notFoundPage :: Html Action
notFoundPage = div [] [ text "not found" ]

--- Search Views

homePage :: State -> Html Action
homePage state =
    div # do
        header_ Nothing
        searchForm state

searchResultPage :: State -> Html Action
searchResultPage state =
    div # do
        header_ Nothing
        searchForm state
        ul [] (searchResult <$> state.io.searchResults)

searchResult :: SearchResult -> Html Action
searchResult (SearchResult {title, id}) =
    li # do
        text title
        button ! onClick (const $ IOAction $ RequestSong id) # do
            text $ show id

searchForm :: State -> Html Action
searchForm state =
    form ! onSubmit (const $ IOAction RequestSearch) # do
        input [ type_ "text", value state.ui.searchQuery, onChange (\f -> UIAction (SearchChange f))] []
        button [ type_ "submit" ] [ text "search" ]

--- Song Views

songPage :: Maybe Song -> Number -> Html Action
songPage Nothing _ = div # text "No song"
songPage (Just s) fontSize =
    div # do
        header_ $ Just songTextSize
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

songTextSize :: Html Action
songTextSize =
    li # do
        button ! onClick (const  (UIAction Increment)) # text "+"
        button ! onClick (const  (UIAction Decrement)) # text "-"
