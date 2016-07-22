module View where

import Prelude (($), (<$>), show, const, (<>))

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, h4, h5, h6, span, i, nav, li, ul, form
                , input, (#), (!), bind)
import Pux.Html.Events (onSubmit, onChange, onMouseMove)
import Pux.Html.Attributes (name, placeholder, type_, value, data_, action, method)
import Pux.Router (link)

import Model (Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(ChordAndLyric, OnlyChord, OnlyLyric), SearchResult(SearchResult), Year(Year), serializeSongSectionName)
import Action (Action(UIAction, PageView), UIAction(SearchChange, SetShowHeader))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage))
import App (State(State), AsyncData(Loading, Loaded, Empty, LoadError), UIState(UIState), IOState(IOState), HeaderVisibility(HideHeader))


view :: State -> Html Action
view state@(State { currentPage } ) = div [] [ page currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page (SongPage _) (State { ui, io }) = songPage io ui
page (SearchResultPage _) state = searchResultPage state
page HomePage state = homePage state
page NotFoundPage _ = notFoundPage

--- Common

type ToolBar = Html Action

header_:: IOState -> UIState -> Html Action
header_ (IOState { searchResults }) (UIState { searchQuery }) =
    header # do
        nav # do
            searchForm searchQuery

songPageHeader :: UIState -> Html Action
songPageHeader (UIState { searchQuery }) =
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
homePage (State { io, ui }) =
    div # do
        header_ io ui

searchResultPage :: State -> Html Action
searchResultPage (State { io, ui }) =
    div # do
        header_ io ui
        searchResultPageContent io ui

searchResultPageContent :: IOState -> UIState -> Html Action
searchResultPageContent (IOState { searchResults: Empty }) _ = ul # text ""
searchResultPageContent (IOState { searchResults: Loading }) _ = ul # text "Loading..."
searchResultPageContent (IOState { searchResults: Loaded s }) _ = ul [] (searchResult <$> s)
searchResultPageContent (IOState { searchResults: (LoadError e) }) _ = ul # text e


searchResult :: SearchResult -> Html Action
searchResult (SearchResult { meta: SongMeta { title, artist, album, year: Year(y) }, desc, id}) =
    li # do
        link ("/song/" <> show id) # do
            h3 # text title
            h4 # text artist
            h5 # text album
            h6 # text (show y)
            i # text desc

searchForm :: String -> Html Action
searchForm q =
    form ! action "/search" ! method "GET" ! onSubmit (const $ PageView (SearchResultPage q)) # do
        input [ name "q", type_ "search", placeholder "Search", value q, onChange (\f -> UIAction (SearchChange f))] []

--- Song Views

songPage :: IOState -> UIState -> Html Action
songPage io ui@(UIState { headerVisibility: HideHeader }) =
    div ! onMouseMove (const $ UIAction SetShowHeader) # do
        songPageContent io ui
songPage io ui =
    div ! onMouseMove (const $ UIAction SetShowHeader) # do
        songPageHeader ui
        songPageContent io ui

songPageContent :: IOState -> UIState -> Html Action
songPageContent (IOState { song: Empty }) _ = main # text ""
songPageContent (IOState { song: Loading }) _ = main # text "Loading Song"
songPageContent (IOState { song: (LoadError e) }) _ = main # text e
songPageContent (IOState { song: Loaded (Song { meta, content }), searchResults }) (UIState { searchQuery }) =
    main # do
        songMeta meta
        songContent content

songMeta :: SongMeta -> Html Action
songMeta (SongMeta { title, artist, album }) =
    header # do
       h1 # text title
       h2 # text artist
       h3 # text album

songContent :: SongContent -> Html Action
songContent (SongContent s) = article [] (songSection <$> s)

songSection :: SongSection -> Html Action
songSection (SongSection {name, lyrics}) =
    section # do
        h4 # text (serializeSongSectionName name)
        p [] (songLyric <$> lyrics)

songLyric :: SongLyric -> Html Action
songLyric (ChordAndLyric chord lyric) =
    span ! data_ "lyrics" lyric # do
        i ! data_ "chord" (show chord) # text ""
        text lyric
songLyric (OnlyChord chord) =
    span ! data_ "chord" (show chord) # do
        i ! data_ "chord" (show chord) # text ""
songLyric (OnlyLyric lyric) =
    span ! data_ "lyrics" lyric # do
        text lyric
