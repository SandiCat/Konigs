module Main exposing (..)

import Task
import Html
import Html.Attributes
import Util
import Window
import MentalMap
import Util.Cmd
import Material
import Material.Layout
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Color
import Material.List
import Material.Spinner
import MyCss
import Array exposing (Array)
import Util
import Dom
import Ports.LocalStorage as LocalStorage
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode


-- MODEL


type alias Model =
    { mdl : Material.Model
    , size : Util.Size
    , files : Array File
    , selection : FileId

    {- Perhaps it should have been `Maybe {files, selection}`, to make
       impossible states impossible. But this makes for easier updates.
       The state of not having any files (before loading them) is represented
       by `files` being empty.
    -}
    }


init : ( Model, Cmd Msg )
init =
    Model Material.model
        (Util.Size 0 0)
        Array.empty
        0
        ! [ Task.perform Resize Window.size
          , loadCmd
          ]


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


type alias Menu r =
    { r
        | files : Array File
        , selection : FileId
        , mdl : Material.Model
    }


toStartingMenu : Menu r -> Menu r
toStartingMenu menu =
    { menu | files = Array.repeat 1 emptyFile, selection = 0 }


isMenuEmpty : Menu r -> Bool
isMenuEmpty =
    .files >> Array.length >> (==) 0


addFile : String -> MentalMap.Model -> Menu r -> Menu r
addFile filename data menu =
    { menu | files = Array.push (initFile filename data) menu.files }


mapFile : FileId -> (File -> File) -> Menu r -> Menu r
mapFile id transform menu =
    case Array.get id menu.files of
        Just old ->
            { menu
                | files =
                    Array.set id (transform old) menu.files
            }

        Nothing ->
            Debug.log "Trying to map nonexistent file!" menu


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
        Debug.crash "Invalid ID!"



{- PERSISTENCE

   On start, show a spinner and attempt retriving files from localStorage.
   If there's no previous saved state or if it's empty, make a new blank file
   and display it.
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
                (\files selection -> { menu | files = files, selection = selection })
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
    | MouseOverFile FileId
    | MouseOutFile FileId
    | Save
    | Loaded ( LocalStorage.Key, LocalStorage.Value )


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
                    Util.Cmd.update
                        ((flip setSelected) model)
                        SelectedMapMsg
                        (MentalMap.update msg_ old)

                Nothing ->
                    model ! [] |> Debug.log "Stray MentalMap message!"

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
                Debug.log "Recieved item from an unknown key" model ! []



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
        , drawer = [ viewMenu model ]
        , header = []
        , tabs = ( [], [] )
        }


viewMenu : Menu r -> Html.Html Msg
viewMenu menu =
    Options.div [ MyCss.mdlClass MyCss.Menu ]
        [ Material.List.ul []
            (Array.indexedMap (viewFile menu) menu.files |> Array.toList)
        , List.indexedMap
            (\i ( msg, iconName, description ) ->
                Button.render MdlMsg
                    [ 1, 0, i ]
                    menu.mdl
                    [ Button.minifab

                    -- I would prefer a round minifab as shown in the demo, but it doesn't work
                    , Button.ripple
                    , Options.onClick msg
                    , Options.attribute <| Html.Attributes.title description
                    ]
                    [ Icon.i iconName ]
            )
            [ ( NewFile, "add", "New file" )
            , ( Save, "sync", "Save" )
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
            , Options.onClick <| ChangeSelection id
            , Options.onMouseOver <| MouseOverFile id
            , Options.onMouseOut <| MouseOutFile id
            ]
            [ if file.renaming then
                Textfield.render MdlMsg
                    [ 0, 0, 0 ]
                    menu.mdl
                    [ Options.onInput (ChangeFilename id)
                    , Options.onBlur (ExitRenaming id)
                    , ExitRenaming id |> Util.onEnter |> Options.attribute
                    , Options.id cssId
                    , Textfield.value file.filename
                    ]
                    []
              else
                Material.List.content []
                    [ Options.div []
                        [ Options.span
                            [ Options.onDoubleClick (EnterRenaming id cssId) ]
                            [ Html.text file.filename ]
                        ]
                    ]
            , if file.mouseOver then
                List.indexedMap
                    (\i ( msg, iconName ) ->
                        Button.render MdlMsg
                            [ 0, 1, i ]
                            menu.mdl
                            [ Button.icon
                            , Options.onClick msg
                            ]
                            [ Icon.i iconName ]
                    )
                    [ ( EnterRenaming id cssId, "border_color" ) ]
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
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
