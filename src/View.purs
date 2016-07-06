module View where

import Prelude (($), (<$>), show, const, (<>))

import Data.Maybe (Maybe(Nothing), maybe, fromMaybe)
import Data.Either (Either(Left, Right))

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, h4, span, i, nav, li, a, button, ul, form
                , input, (#), (!), bind)
import Pux.Router (link)
import Pux.Html.Events (onClick, onSubmit, onChange)
import Pux.Html.Attributes (type_, value, data_, href)

import Model (Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(SongLyric), SearchResult(SearchResult))
import Action (Action(UIAction, IOAction), IOAction(RequestSearch, RequestSong), UIAction(Increment, Decrement, SearchChange))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage))
import App (State, SongState(Loading, Loaded, Empty), UIState, IOState)


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
            ul [] (searchResult <$> searchResults)

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

searchResult :: SearchResult -> Html Action
searchResult (SearchResult {title, id}) =
    li # do
        a ! href ("/song/" <> show id) ! onClick (const $ IOAction $ RequestSong id) # do
            text title

searchForm :: String -> Html Action
searchForm q =
    form ! onSubmit (const $ IOAction RequestSearch) # do
        input [ type_ "search", value q, onChange (\f -> UIAction (SearchChange f))] []

--- Song Views

songPage :: IOState -> UIState -> Html Action
songPage io ui =
    div # do
        songPageHeader ui
        songPageContent io ui

songPageContent :: IOState -> UIState -> Html Action
songPageContent { song = Empty } _ = div # text ""
songPageContent { song = Loading } _ = div # text "Loading Song"
songPageContent { song = Loaded (Left e) } _ = div # text (show e)
songPageContent { song = Loaded (Right (Song { meta, content })), searchResults } { searchQuery }=
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

songTextSize :: Html Action
songTextSize =
    li # do
        button ! onClick (const  (UIAction Increment)) # text "+"
        button ! onClick (const  (UIAction Decrement)) # text "-"
