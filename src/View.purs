module View where

import Prelude (($), (<$>), show, const)

import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Either (Either(Left, Right))

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, span, b, nav, li, button, ul, form
                , input, (#), (!), bind)
import Pux.CSS (style, rem, fontSize)
import Pux.Router (link)
import Pux.Html.Events (onClick, onSubmit, onChange)
import Pux.Html.Attributes (type_, value, data_)

import Model (Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(SongLyric), SearchResult(SearchResult))
import Action (Action(UIAction, IOAction), IOAction(RequestSearch, RequestSong), UIAction(Increment, Decrement, SearchChange))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage))
import App (State, SongState(Loading, Loaded, Empty))


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
header_ toolbar =
    header # do
        nav # do
            link "/" #
                text "Home"
            fromMaybe (text "") toolbar

--- NotFound view

notFoundPage :: Html Action
notFoundPage =
    div # do
        header_ Nothing
        text "not found"

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

songPage :: SongState -> Number -> Html Action
songPage Empty _ = div # text ""
songPage Loading _ = div # text "Loading Song"
songPage (Loaded (Left e)) _ = div # text (show e)
songPage (Loaded (Right (Song s))) fontSize =
    div # do
        header_ $ Just songTextSize
        main # do
            songMeta s.meta
            songContent s.content fontSize

songMeta :: SongMeta -> Html Action
songMeta (SongMeta { title, artist, album }) =
    header # do
       h1 # text title
       h2 # text artist
       maybe (text "") (\a -> h3 # text a) album

songContent :: SongContent -> Number -> Html Action
songContent (SongContent s) fs = article [ style $ fontSize (rem fs) ] (songSection <$> s)

songSection :: SongSection -> Html Action
songSection (SongSection {name, lyrics}) =
    section # do
        h2 # text (show name)
        p [] (songLyric <$> lyrics)

songLyric :: SongLyric -> Html Action
songLyric (SongLyric {lyric, chord}) =
    span ! data_ "lyrics" l # do
        b ! data_ "chord" c # text ""
        text l
    where
        c = maybe "" show chord
        l = fromMaybe "" lyric

songTextSize :: Html Action
songTextSize =
    li # do
        button ! onClick (const  (UIAction Increment)) # text "+"
        button ! onClick (const  (UIAction Decrement)) # text "-"
