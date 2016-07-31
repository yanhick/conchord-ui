module View where

import Prelude (($), (<$>), show, const, (<>))
import Data.Tuple (fst, snd)
import Data.Either (Either(Left, Right))

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, h4, h5, h6, span, i, nav, li, ul, form
                , input, textarea, button, aside, (#), (!), bind)
import Pux.Html.Events (onSubmit, onChange, onMouseMove)
import Pux.Html.Attributes (name, placeholder, type_, value, data_, action, method, className)
import Pux.Router (link)

import Model (Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(ChordAndLyric, OnlyChord, OnlyLyric), SearchResult(SearchResult), Year(Year), serializeSongSectionName, serializeSong)
import Action (Action(UIAction, PageView, IOAction), UIAction(SearchChange, SetShowHeader, NewSongChange, UpdateSongChange), IOAction(SubmitNewSong, SubmitUpdateSong, SubmitDeleteSong))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage, NewSongPage, UpdateSongPage))
import App (State(State), AsyncData(Loading, Loaded, Empty, LoadError), UIState(UIState), IOState(IOState), HeaderVisibility(HideHeader), ValidatedSong)


view :: State -> Html Action
view state@(State { currentPage } ) = div [] [ page currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page (SongPage _) (State { ui, io, currentPage }) = songPage currentPage io ui
page (SearchResultPage _) state = searchResultPage state
page HomePage state = homePage state
page NewSongPage state = newSongPage state
page (UpdateSongPage _) state = updateSongPage state
page NotFoundPage _ = notFoundPage

--- Common

type ToolBar = Html Action

header_:: IOState -> UIState -> Html Action
header_ (IOState { searchResults }) (UIState { searchQuery }) =
    header # do
        nav # do
            searchForm searchQuery
            link "/new" # do
                text "add a new song"

songPageHeader :: Route -> UIState -> Html Action
songPageHeader (SongPage id) (UIState { searchQuery }) =
    header # do
        nav # do
            searchForm searchQuery
            link ("/update/" <> show id) # do
                text "edit this song"
            link ("/new") # do
                text "add a new song"
            deleteSongForm id

songPageHeader _ (UIState { searchQuery }) =
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
        input [ name "q", type_ "search", placeholder "Search a song, artist or album", value q, onChange (\f -> UIAction (SearchChange f))] []


--- New Song views

newSongPageHeader :: Html Action
newSongPageHeader = do
    nav # do
        h1 # text "Add a new song"

newSongPage :: State -> Html Action
newSongPage (State { io: IOState { newSong }, ui }) =
    div ! className "editor" # do
        aside # do
            newSongForm $ fst newSong
        main # do
            render $ snd newSong
    where
        render (Right (Song { meta, content })) = do
            songMeta meta
            songContent content
        render (Left e) = text e

newSongForm :: String -> Html Action
newSongForm song =
    form ! action "/new" ! method "POST" ! onSubmit (const $ IOAction SubmitNewSong) # do
        textarea [ name "song", type_ "text", value song, onChange (\e -> UIAction (NewSongChange e)) ] []
        input [ type_ "submit", value "add this new song" ] []


--- Update Song views

updateSongPage :: State -> Html Action
updateSongPage (State { io: IOState { newSong } }) =
    div ! className "editor" # do
        aside # do
            updateSongForm $ fst newSong
        main # do
            render $ snd newSong
    where
        render (Right (Song { meta, content })) = do
            songMeta meta
            songContent content
        render (Left e) = text e

updateSongForm :: String -> Html Action
updateSongForm song =
    form ! action "/update" ! method "PUT" ! onSubmit (const $ IOAction SubmitUpdateSong) # do
        textarea [ name "song", type_ "text", onChange (\e -> UIAction (UpdateSongChange e)), value song ] []
        input [ type_ "submit", value "edit this song" ] []

--- Delete Song views

deleteSongForm :: Int -> Html Action
deleteSongForm id =
    form ! action "/song/api" ! method "DELETE" ! onSubmit (const $ IOAction (SubmitDeleteSong id)) # do
        input [ type_ "submit", value "delete this song" ] []

--- Song Views

songPage :: Route -> IOState -> UIState -> Html Action
songPage _ io ui@(UIState { headerVisibility: HideHeader }) =
    div ! onMouseMove (const $ UIAction SetShowHeader) # do
        songPageContent io ui
songPage r io ui =
    div ! onMouseMove (const $ UIAction SetShowHeader) # do
        songPageHeader r ui
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
