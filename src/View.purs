module View where

import Prelude (($), (<$>), show, const, (<>))

import Data.Maybe (maybe, fromMaybe)
import Data.Either (Either(Left, Right))

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, h4, h5, h6, span, i, nav, li, ul, form
                , input, (#), (!), bind)
import Pux.Html.Events (onSubmit, onChange)
import Pux.Html.Attributes (placeholder, type_, value, data_, action, method)
import Pux.Router (link)

import Model (Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(SongLyric), SearchResult(SearchResult), Year(Year))
import Action (Action(UIAction, PageView), UIAction(SearchChange))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage))
import App (State, AsyncData(Loading, Loaded, Empty), UIState, IOState)


view :: State -> Html Action
view state = div [] [ page state.currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page (SongPage _) { ui, io }= songPage io ui
page (SearchResultPage _) state = searchResultPage state
page HomePage state = homePage state
page NotFoundPage _ = notFoundPage

--- Common

type ToolBar = Html Action

header_:: IOState -> UIState -> Html Action
header_ { searchResults } { searchQuery } =
    header # do
        nav # do
            searchForm searchQuery

songPageHeader :: UIState -> Html Action
songPageHeader { searchQuery } =
    header # do
        nav # do
            searchForm searchQuery

--- NotFound view

notFoundPage :: Html Action
notFoundPage =
    div # do
        text "not found"

--- Search Views

homePage :: State -> Html Action
homePage { io, ui }=
    div # do
        header_ io ui

searchResultPage :: State -> Html Action
searchResultPage { io, ui }=
    div # do
        header_ io ui
        searchResultPageContent io ui

searchResultPageContent :: IOState -> UIState -> Html Action
searchResultPageContent { searchResults: Empty } _ = div # text ""
searchResultPageContent { searchResults: Loading } _ = div # text "Loading..."
searchResultPageContent { searchResults: Loaded (Right s) } _ = ul [] (searchResult <$> s)
searchResultPageContent { searchResults: Loaded (Left _) } _ = div # text "Could not find results"


searchResult :: SearchResult -> Html Action
searchResult (SearchResult { meta: SongMeta { title, artist, album, year: Year(y) }, desc, id}) =
    li # do
        link ("/song/" <> show id) # do
            h3 # text title
            h4 # text artist
            h5 # text (fromMaybe "" album)
            h6 # text (show y)
            i # text desc

searchForm :: String -> Html Action
searchForm q =
    form ! action "/search" ! method "GET" ! onSubmit (const $ PageView (SearchResultPage q)) # do
        input [ type_ "search", placeholder "Search", value q, onChange (\f -> UIAction (SearchChange f))] []

--- Song Views

songPage :: IOState -> UIState -> Html Action
songPage io ui =
    div # do
        songPageHeader ui
        songPageContent io ui

songPageContent :: IOState -> UIState -> Html Action
songPageContent { song: Empty } _ = div # text ""
songPageContent { song: Loading } _ = div # text "Loading Song"
songPageContent { song: Loaded (Left e) } _ = div # text (show e)
songPageContent { song: Loaded (Right (Song { meta, content })), searchResults } { searchQuery }=
    main # do
        songMeta meta
        songContent content

songMeta :: SongMeta -> Html Action
songMeta (SongMeta { title, artist, album }) =
    header # do
       h1 # text title
       h2 # text artist
       maybe (text "") (\a -> h3 # text a) album

songContent :: SongContent -> Html Action
songContent (SongContent s) = article [] (songSection <$> s)

songSection :: SongSection -> Html Action
songSection (SongSection {name, lyrics}) =
    section # do
        h4 # text (show name)
        p [] (songLyric <$> lyrics)

songLyric :: SongLyric -> Html Action
songLyric (SongLyric {lyric, chord}) =
    span ! data_ "lyrics" l # do
        i ! data_ "chord" c # text ""
        text l
    where
        c = maybe "" show chord
        l = fromMaybe "" lyric
