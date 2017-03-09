module View where

import Data.String (take)
import Data.Foldable (foldMap)
import Prelude (($), (<$>), show, const, (<>), not, pure)
import Data.Tuple (fst, snd)
import Data.Either (Either(Left, Right), isLeft)

import Pux.Html (Html, section, div, main, p, text, header, article
                , h1, h2, h3, h4, h5, h6, span, i, nav, li, ul, form
                , input, textarea, aside, (#), (##), (!), bind, label, footer)
import Pux.Html.Events (onSubmit, onChange, onClick)
import Pux.Html.Attributes (id_, htmlFor, checked, name, placeholder, type_, value, data_, action, method, className, disabled, formAction)
import Pux.Router (link)

import Model (DBSong(DBSong), Song(Song), SongMeta(SongMeta), SongContent(SongContent), SongSection(SongSection), SongLyric(ChordAndLyric, OnlyChord, OnlyLyric), Year(Year), SongSectionName(Chorus), serializeSongSectionName, ChordPlacement(InsideWord, BetweenWord), SongsList, SearchResults)
import Action (Action(UIAction, PageView, IOAction), UIAction(ToggleShowMenus, ToggleShowSongSectionName, ToggleShowDuplicatedChorus, ToggleShowSongMeta, SearchChange, NewSongChange, UpdateSongChange), IOAction(SubmitNewSong, SubmitUpdateSong, SubmitDeleteSong))
import Route (Route (SongPage, SearchResultPage, HomePage, NotFoundPage, NewSongPage, UpdateSongPage))
import App (State(State), AsyncData(Loading, Loaded, Empty, LoadError), SongUIState(SongUIState), UIState(UIState), IOState(IOState))


view :: State -> Html Action
view state@(State { currentPage } ) = div [] [ page currentPage state ]

--- App Routing

page :: Route -> State -> Html Action
page r s = do
    div ! className "min-vh-100 site flex flex-column" ## page' r s
    where
        page' (SongPage id) (State { ui, io, currentPage }) = songPage id io ui
        page' (SearchResultPage _) state = searchResultPage state
        page' HomePage state = homePage state
        page' NewSongPage state = newSongPage state
        page' (UpdateSongPage id) state = updateSongPage id state
        page' NotFoundPage _ = notFoundPage

--- Common

type ToolBar = Html Action

header_:: IOState -> UIState -> Html Action
header_ (IOState { searchResults }) (UIState { searchQuery }) =
    header # do
        searchForm searchQuery

footer_ :: Html Action
footer_ =
    footer # do
        nav ! className footerContainer # do
            link ("/new") ! className linkUI # do
                text "add a new song"

songPageFooter :: Int -> Html Action
songPageFooter id =
    footer # do
        nav ! className footerContainer # do
            link ("/update/" <> show id) ! className linkUI # do
                text "edit this song"
            link ("/new") ! className linkUI # do
                text "add a new song"
            deleteSongForm id

songPageHeader :: Int -> UIState -> Html Action
songPageHeader id (UIState { searchQuery, songUIState: SongUIState { showSongMeta, showDuplicatedChorus, showSongSectionName, showMenus } } ) =
    header # do
        nav ! className "flex flex-wrap bg-dark-gray" # do
            link ("/") ! className (linkUI <> " bg-white") # do
                text "<<"
            form ! className "flex flex-wrap" ! action ("/song/" <> show id) ! method "GET" # do
                input [ className "dn", name "hide-song-meta", type_ "checkbox", id_ "song-meta-toggle", checked $ not showSongMeta, onChange (\_ -> UIAction ToggleShowSongMeta) ] []
                label [ className checkboxUI, htmlFor "song-meta-toggle" ] [ text "hide song meta" ]
                input [ className "dn", name "hide-duplicated-chords", type_ "checkbox", id_ "duplicated-chords-toggle", checked $ not showDuplicatedChorus, onChange (\_ -> UIAction ToggleShowDuplicatedChorus) ] []
                label [ className checkboxUI, htmlFor "duplicated-chords-toggle" ] [ text "hide song duplicated chorus" ]
                input [ className "dn", name "hide-song-section-name", type_ "checkbox", id_ "song-section-name-toggle", checked $ not showSongSectionName, onChange (\_ -> UIAction ToggleShowSongSectionName) ] []
                label [ className checkboxUI, htmlFor "song-section-name-toggle" ] [ text "hide song section name" ]
                input [ className "dn", name "hide-menus", type_ "checkbox", id_ "menus-toggle", checked $ not showMenus, onChange (\_ -> UIAction ToggleShowMenus) ] []
                label [ className checkboxUI, htmlFor "menus-toggle" ] [ text "hide menus" ]
                input [ className (linkUI <> resetInput <> "blue"), type_ "submit", value "update" ] []

songPageMinimalHeader :: Int -> UIState -> Html Action
songPageMinimalHeader id (UIState { searchQuery, songUIState: SongUIState { showSongMeta, showDuplicatedChorus, showSongSectionName, showMenus } } ) =
    header # do
        nav ! className "flex flex-wrap" # do
            form !className "flex" ! action ("/song/" <> show id) ! method "GET" # do
                input [ className "dn", name "hide-menus", type_ "checkbox", id_ "menus-toggle", checked $ not showMenus, onChange (\_ -> UIAction ToggleShowMenus) ] []
                label [ className checkboxUI, htmlFor "menus-toggle" ] [ text "show menus" ]
                input [ type_ "submit", value "update" ] []

--- NotFound view

notFoundPage :: Array (Html Action)
notFoundPage = pure $ text "not found"

--- Search Views

homePage :: State -> Array (Html Action)
homePage (State { io: io@IOState { songsList }, ui }) = [
        header_ io ui,
        homepageBody songsList,
        footer_
    ]

homepageBody :: AsyncData SongsList -> Html Action
homepageBody songsList =
    main ! className "flex-auto" # do
        ul ! className "flex flex-wrap flex-auto justify-center list pa3 serif" ## searchResultPageContent songsList

searchResultPage :: State -> Array (Html Action)
searchResultPage (State { io: io@IOState { searchResults }, ui }) = [
        header_ io ui,
        ul ! className "flex flex-wrap flex-auto justify-center list pa3 serif" ## searchResultPageContent searchResults,
        footer_
    ]

searchResultPageContent :: AsyncData SearchResults -> Array (Html Action)
searchResultPageContent Empty = [text ""]
searchResultPageContent Loading = [h1 ! className loadingText # text "LOADING..."]
searchResultPageContent (Loaded s) = searchResult <$> s
searchResultPageContent (LoadError e) = [text e]


searchResult :: DBSong -> Html Action
searchResult (DBSong { id, song: Song { meta: SongMeta { title, artist, album, year: Year(y) }, content: SongContent (content) } }) =
    li ! className "dim mw6 left hover-white" # do
        link ("/song/" <> id) ! className "dark-gray dib no-underline ma2 pa3" # do
            h1 ! className "sans-serif normal f3 pamax-width-1 pa2 ma0 mb1 white bg-dark-gray white" # text title
            h2 ! className "sans-serif normal f5 pa1 white pa2 bg-dark-gray ma0 white" # text artist
            i  ! className "block py3 content-ellipsis-after pv2 db" # text (content' content)
    where
        content' c = take 200 (foldMap (\(SongSection {lyrics}) -> foldMap serializeLyric lyrics) c)
        serializeLyric (ChordAndLyric _ l) = l
        serializeLyric (OnlyLyric l) = l
        serializeLyric (OnlyChord _) = ""


searchForm :: String -> Html Action
searchForm q =
    form ! className "flex justify-center" ! action "/search" ! method "GET" ! onSubmit (const $ PageView (SearchResultPage q)) # do
        input [ className "w-50 f5 pa3 ma3 ba b--dark-gray br-pill", name "q", type_ "search", placeholder "Search a song, artist or album", value q, onChange (\f -> UIAction (SearchChange f))] []


--- New Song views

newSongPageHeader :: Html Action
newSongPageHeader = do
    nav ! className "flex flex-wrap bg-dark-gray" # do
        h1 # text "Add a new song"

newSongPage :: State -> Array (Html Action)
newSongPage (State { io: IOState { newSong }, ui }) =
        pure $ div ! className "flex flex-row" # do
            aside ! className "vh-100 w-50 flex" # do
                newSongForm (isLeft (snd newSong)) (fst newSong)
            main ! className songContainer # do
                render $ snd newSong
    where
        render (Right (Song { meta, content })) = do
            songMeta meta
            songContent true content
        render (Left e) = text e

newSongForm :: Boolean -> String -> Html Action
newSongForm disable song =
    form ! className "flex flex-column flex-auto" ! action "/new" ! method "POST" ! onSubmit (const $ IOAction SubmitNewSong) # do
        textarea [ className "flex-auto pa3 bg-dark-gray white bn", name "song", type_ "text", value song, onChange (\e -> UIAction (NewSongChange e)) ] []
        input [ className (linkUI <> resetInput), type_ "submit", value "add this new song", disabled disable ] []


--- Update Song views

updateSongPage :: Int -> State -> Array (Html Action)
updateSongPage id (State { io: IOState { updateSong } }) =
    pure $ div ! className "flex flex-row" # do
        aside ! className "vh-100 w-50 flex" # do
            updateSongForm id (isLeft (snd updateSong)) (fst updateSong)
        main ! className (songContainer <> " w-50") # do
            render $ snd updateSong
    where
        render (Right (Song { meta, content })) = do
            songMeta meta
            songContent true content
        render (Left e) = text e

updateSongForm :: Int -> Boolean -> String -> Html Action
updateSongForm id disable song =
    form ! className "flex flex-column flex-auto" ! action ("/update/" <> show id) ! method "POST" ! onSubmit (const $ IOAction SubmitUpdateSong) # do
        textarea [ className "flex-auto pa3 bg-dark-gray white bn", name "song", type_ "text", onChange (\e -> UIAction (UpdateSongChange e)), value song ] []
        input [ className (linkUI <> resetInput), type_ "submit", value "edit this song", disabled disable ] []

--- Delete Song views

deleteSongForm :: Int -> Html Action
deleteSongForm id =
    form ! className "flex" ! action "/song/api" ! method "DELETE" ! onSubmit (const $ IOAction (SubmitDeleteSong id)) # do
        input [ className (linkUI <> "red " <> resetInput), type_ "submit", value "delete this song" ] []

--- Song Views

songPage :: Int -> IOState -> UIState -> Array (Html Action)
songPage id io ui@(UIState { songUIState: SongUIState { showMenus } }) = songPage' showMenus
    where
        songPage' true = [
            songPageHeader id ui,
            songPageContent io ui,
            songPageFooter id
        ]
        songPage' false = [
            songPageMinimalHeader id ui,
            songPageContent io ui
        ]

songPageContent :: IOState -> UIState -> Html Action
songPageContent (IOState { song: Empty }) _ = main # text ""
songPageContent (IOState { song: Loading }) _ = main # do
    h1 ! className loadingText # do
        text "LOADING..."
songPageContent (IOState { song: (LoadError e) }) _ = main # text e
songPageContent (IOState { song: Loaded (Song { meta, content })}) (UIState { searchQuery, songUIState: SongUIState { showSongMeta, showDuplicatedChorus, showSongSectionName } } ) =
    main [className $ songContainer <> hideChorusesClass (not showDuplicatedChorus)] [content' showSongMeta]
    where
        content' true = do
            songMeta meta
            songContent showSongSectionName content
        content' false = do
            songContent showSongSectionName content
        hideChorusesClass true = "hide-duplicated-chorus"
        hideChorusesClass false = ""

songMeta :: SongMeta -> Html Action
songMeta (SongMeta { title, artist, album }) =
    header
        ! className "lh-title tr pb2 mb2 bb b--dark-gray"
        # do
            h1 ! className "f3 normal ma0" # text title
            h2 ! className "f5 normal di content-dash-after" # text artist
            h3 ! className "f5 normal di" # text album

songContent :: Boolean -> SongContent -> Html Action
songContent showSongSectionName (SongContent s) = article [] ((songSection showSongSectionName) <$> s)

songSection :: Boolean -> SongSection -> Html Action
songSection showName (SongSection {name, lyrics}) =
    section
        ! className ("column-break-inside-avoid " <> sectionClassNames name)
        ! data_ "section" (serializeSongSectionName name)
        ## sectionContent showName
    where
        sectionContent true = [
            h4 ! className "ma0 ttu f6" # text (serializeSongSectionName name),
            sectionText
        ]
        sectionContent false = [
            sectionText
        ]
        sectionText = p
            ! className "ma0 ml2 justify"
            ## (songLyric <$> lyrics)

        sectionClassNames Chorus = "bg-near-white pa2"
        sectionClassNames _ = ""

songLyric :: SongLyric -> Html Action
songLyric (ChordAndLyric chord lyric) =
    span ! className "chord-and-lyrics" ! data_ "lyrics" lyric ! data_ "chord-placement" (chordPlacement chord) # do
        i ! className "chord-name" ! data_ "chord" (showChord chord) # text ""
        text lyric
    where
        showChord (InsideWord c) = show c
        showChord (BetweenWord c) = show c

        chordPlacement (InsideWord _) = "inside"
        chordPlacement (BetweenWord _) = "between"


songLyric (OnlyChord chord) =
    span ! className "chord-and-lyrics" ! data_ "chord" (show chord) # do
        i ! className "chord-name" ! data_ "chord" (show chord) # text ""
songLyric (OnlyLyric lyric) =
    span ! className "chord-and-lyrics" ! data_ "lyrics" lyric # do
        text lyric

--- Style Utils

textUI :: String
textUI = " ttu f6 no-underline"

loadingText :: String
loadingText = " f3 normal sans-serif "

boxUI :: String
boxUI = " pa3 "

interactiveUI :: String
interactiveUI = " pointer hover-bg-blue hover-white link "

linkUI :: String
linkUI = textUI <> boxUI <> interactiveUI

checkboxUI :: String
checkboxUI = linkUI <> " white"

resetInput :: String
resetInput = " bn bg-transparent "

footerContainer :: String
footerContainer = " flex flex-wrap bt b--dark-gray "

songContainer :: String
songContainer = " column-container flex-auto song-content-line-height pa3 ma2 "
