module Main exposing (..)

import Task
import Html
import Html.Attributes
import Util
import Window
import MentalMap
import Material
import Material.Layout
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Tabs as Tabs
import Material.Color
import Material.List
import Material.Spinner
import Material.Progress
import MyCss
import Array exposing (Array)
import Util
import Dom
import Ports.LocalStorage as LocalStorage
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode
import Array.Extra
import Keyboard.Extra
import Time
import Css
import Markdown


-- MODEL


type alias Model =
    { mdl : Material.Model
    , size : Util.Size

    -- Menu:
    , loading : Bool
    , files : Array File
    , selection : FileId

    {- Perhaps it should have been `Maybe {files, selection}`, to make
       impossible states impossible. But this makes for easier updates.
       The state of not having any files (before loading them) is represented
       by `files` being empty.
    -}
    , selectedTab : Int
    }


init : ( Model, Cmd Msg )
init =
    Model Material.model
        (Util.Size 0 0)
        True
        Array.empty
        0
        0
        ! [ Task.perform Resize Window.size
          , loadCmd
          ]



-- FILES


type alias FileId =
    Int


type alias File =
    { filename : String
    , renaming : Bool
    , mouseOver : Bool
    , data : MentalMap.Model
    }


decodeFile : Decode.Decoder File
decodeFile =
    Decode.succeed initFile
        |: (Decode.field "filename" Decode.string)
        |: (Decode.field "data" MentalMap.decode)


encodeFile : File -> Encode.Value
encodeFile file =
    Encode.object
        [ ( "filename", Encode.string file.filename )
        , ( "data", MentalMap.encode file.data )
        ]


initFile : String -> MentalMap.Model -> File
initFile filename data =
    File filename False False data


emptyFile : File
emptyFile =
    initFile "New File" MentalMap.emptyInit



-- MENU


type alias Menu r =
    { r
        | files : Array File
        , selection : FileId
        , loading : Bool
        , mdl : Material.Model
    }


toStartingMenu : Menu r -> Menu r
toStartingMenu menu =
    { menu | files = Array.repeat 1 emptyFile, selection = 0, loading = False }


isMenuEmpty : Menu r -> Bool
isMenuEmpty =
    .files >> Array.length >> (==) 0


addFile : String -> MentalMap.Model -> Menu r -> Menu r
addFile filename data menu =
    { menu | files = Array.push (initFile filename data) menu.files }


deleteFile : FileId -> Menu r -> Menu r
deleteFile id menu =
    if isMenuEmpty menu then
        toStartingMenu menu
    else
        { menu
            | selection =
                if menu.selection == id then
                    0
                else
                    menu.selection
            , files = Array.Extra.removeAt id menu.files
        }


mapFile : FileId -> (File -> File) -> Menu r -> Menu r
mapFile id transform menu =
    case Array.get id menu.files of
        Just old ->
            { menu
                | files =
                    Array.set id (transform old) menu.files
            }

        Nothing ->
            Util.crashLog "Trying to map nonexistent file!" menu


getSelected : Menu r -> Maybe MentalMap.Model
getSelected menu =
    Array.get menu.selection menu.files
        |> Maybe.map .data


setSelected : MentalMap.Model -> Menu r -> Menu r
setSelected mentalMap menu =
    mapFile menu.selection (\old -> { old | data = mentalMap }) menu


changeSelection : FileId -> Menu r -> Menu r
changeSelection newId menu =
    if newId >= 0 && newId < Array.length menu.files then
        { menu | selection = newId }
    else
        Util.crashLog "Invalid ID!" menu



{- PERSISTENCE

   On start, show a spinner and attempt retriving files from localStorage.
   If there's no previous saved state or if it's empty, make a new blank file
   and display it.

   For now, all of the state is both saved and loaded at once. This is simple,
   but doesn't allow for multiple sessions nor loading/saving individual files.
-}


localStorageKey : LocalStorage.Key
localStorageKey =
    "data"


save : Menu r -> Cmd Msg
save menu =
    Encode.object
        [ ( "files", Array.map encodeFile menu.files |> Encode.array )
        , ( "selection", Encode.int menu.selection )
        ]
        |> (,) localStorageKey
        |> LocalStorage.storageSetItem


loadCmd : Cmd Msg
loadCmd =
    LocalStorage.storageGetItem localStorageKey


load : LocalStorage.Value -> Menu r -> Menu r
load value menu =
    case
        Decode.decodeValue
            (Decode.succeed
                (\files selection ->
                    { menu
                        | files = files
                        , selection = selection
                        , loading = False
                    }
                )
                |: (Decode.field "files" <| Decode.array decodeFile)
                |: (Decode.field "selection" Decode.int)
            )
            value
    of
        Ok newMenu ->
            if isMenuEmpty newMenu then
                toStartingMenu menu
            else
                newMenu

        Err error ->
            {- This could either mean that the decoder is invalid or that there
               were no files saved beforehand. Either way, begin with an empty file.
               It would be nice to only log on decoder errors, but there's no way
               to differentiate the two scenarios.
            -}
            toStartingMenu menu |> Debug.log error



-- UPDATE


type Msg
    = NoOp
    | Resize Util.Size
    | SelectedMapMsg MentalMap.Msg
    | MdlMsg (Material.Msg Msg)
    | ChangeSelection FileId
    | EnterRenaming FileId String
    | ExitRenaming FileId
    | ChangeFilename FileId String
    | NewFile
    | DeleteFile FileId
    | MouseOverFile FileId
    | MouseOutFile FileId
    | Save
    | Loaded ( LocalStorage.Key, LocalStorage.Value )
    | SelectTab Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Resize size ->
            { model | size = size } ! []

        SelectedMapMsg msg_ ->
            case getSelected model of
                Just old ->
                    Util.updateCmd
                        ((flip setSelected) model)
                        SelectedMapMsg
                        (MentalMap.update msg_ old)

                Nothing ->
                    model ! [] |> Util.crashLog "Stray MentalMap message!"

        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        ChangeSelection id ->
            changeSelection id model ! []

        EnterRenaming id cssId ->
            mapFile id (\file -> { file | renaming = True }) model
                ! [ Dom.focus cssId |> Task.attempt (\_ -> NoOp) ]

        ExitRenaming id ->
            mapFile id (\file -> { file | renaming = False }) model ! []

        ChangeFilename id filename ->
            mapFile id (\file -> { file | filename = filename }) model ! []

        NewFile ->
            addFile "New File" MentalMap.emptyInit model ! []

        DeleteFile id ->
            deleteFile id model ! []

        MouseOverFile id ->
            mapFile id (\file -> { file | mouseOver = True }) model ! []

        MouseOutFile id ->
            mapFile id (\file -> { file | mouseOver = False }) model ! []

        Save ->
            model ! [ save model ]

        Loaded ( key, value ) ->
            if key == localStorageKey then
                load value model ! []
            else
                Util.crashLog "Recieved item from an unknown key" model ! []

        SelectTab i ->
            { model | selectedTab = i } ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    Material.Layout.render MdlMsg
        model.mdl
        []
        { main =
            [ case getSelected model of
                Just mentalMap ->
                    MentalMap.view model.size mentalMap
                        |> Html.map SelectedMapMsg

                Nothing ->
                    Options.div
                        [ Options.center
                        , MyCss.mdlClass MyCss.MaxSize
                        ]
                        [ Material.Spinner.spinner
                            [ Material.Spinner.active True ]
                        ]
            ]
        , drawer = [ viewDrawer model ]
        , header = []
        , tabs = ( [], [] )
        }


viewDrawer : Model -> Html.Html Msg
viewDrawer model =
    Options.div []
        [ Tabs.render MdlMsg
            [ 0 ]
            model.mdl
            [ Tabs.ripple
            , Tabs.onSelectTab SelectTab
            , Tabs.activeTab model.selectedTab
            ]
            [ Tabs.label
                [ Options.center ]
                [ Icon.i "info_outline"
                , spacer 4
                , Html.text "Help"
                ]
            , Tabs.label
                [ Options.center ]
                [ Icon.i "folder_open"
                , spacer 4
                , Html.text "Files"
                ]
            ]
            [ case model.selectedTab of
                0 ->
                    helpTab

                1 ->
                    viewMenu model

                _ ->
                    Util.crashLog "unknown model.selectedTab value" helpTab
            ]
        ]


spacer : Float -> Html.Html a
spacer width =
    Html.span [ MyCss.style [ Css.width <| Css.px width ] ] []


helpTab : Html.Html a
helpTab =
    (Options.styled Markdown.toHtml) [ MyCss.mdlClass MyCss.Help, Typo.body1 ] """
##### Instructions:
* Double click to make a new node
* Click a on node's text to edit it
* Drag from one node to another to connect them
* Hold `shift` while connecting for a directed edge
* Drag the background to pan the view

##### Welcome to Konigs!

Use it to map your thoughts. A node is a *thing*: a person, event, image, diagram, formula, object, idea... whatever. By connecting related things, you begin to understand the big picture. Konigs will automatically organize your nodes.
"""


viewMenu : Menu r -> Html.Html Msg
viewMenu menu =
    Options.div []
        [ if menu.loading then
            Material.Progress.indeterminate
          else
            Options.div [] []
        , Material.List.ul []
            (Array.indexedMap (viewFile menu) menu.files |> Array.toList)
        , List.indexedMap
            (\i ( msg, iconName, description ) ->
                Button.render MdlMsg
                    [ 1, 1, 0, i ]
                    menu.mdl
                    [ Button.minifab

                    -- I would prefer a round minifab as shown in the demo, but it doesn't work
                    , Button.ripple
                    , Options.when menu.loading Button.disabled
                    , Options.onClick msg
                    , Options.attribute <| Html.Attributes.title description
                    ]
                    [ Icon.i iconName ]
            )
            [ ( NewFile, "add", "New file" )
            , ( Save, "save", "Save" )
            ]
            |> Options.div [ MyCss.mdlClass MyCss.MenuButtons ]
        ]


viewFile : Menu r -> FileId -> File -> Html.Html Msg
viewFile menu id file =
    let
        cssId =
            "file-" ++ toString id
    in
        Material.List.li
            [ Material.Color.color Material.Color.Red Material.Color.S200
                |> Material.Color.background
                |> Options.when (menu.selection == id)
            , MyCss.mdlClass MyCss.File
            , Options.onMouseOver <| MouseOverFile id
            , Options.onMouseOut <| MouseOutFile id
            ]
            [ if file.renaming then
                Textfield.render MdlMsg
                    [ 1, 0, 0, id ]
                    menu.mdl
                    [ Options.onInput (ChangeFilename id)
                    , Options.onBlur (ExitRenaming id)
                    , ExitRenaming id
                        |> Util.onSpecificKeyPress Keyboard.Extra.Enter
                        |> Options.attribute
                    , Options.id cssId
                    , Textfield.value file.filename
                    ]
                    []
              else
                Material.List.content
                    [ Options.onDoubleClick (EnterRenaming id cssId)
                    , Options.onClick <| ChangeSelection id
                    ]
                    [ Html.text file.filename ]
            , if file.mouseOver && not file.renaming then
                List.indexedMap
                    (\i ( msg, iconName, description ) ->
                        Button.render MdlMsg
                            [ 1, 0, 1, i ]
                            menu.mdl
                            [ Button.icon
                            , Options.onClick msg
                            , Options.attribute <| Html.Attributes.title description
                            ]
                            [ Icon.i iconName ]
                    )
                    [ ( EnterRenaming id cssId, "border_color", "Rename" )
                    , ( DeleteFile id, "delete", "Delete" )
                    ]
                    |> Options.div []
              else
                Options.div [] []
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , LocalStorage.storageGetItemResponse Loaded
        , case getSelected model of
            Just mentalMap ->
                MentalMap.subscriptions mentalMap
                    |> Sub.map SelectedMapMsg

            Nothing ->
                Sub.none
        , Time.every (5 * Time.second) (always Save)
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
