module Main exposing (..)

import Task
import Html
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
import MyCss
import Array exposing (Array)
import Util
import Dom


-- MODEL


type alias Model =
    { mdl : Material.Model
    , size : Util.Size
    , files : Array File
    , selection : FileId
    }


type alias FileId =
    Int


type alias File =
    { filename : String
    , renaming : Bool
    , mouseOver : Bool
    , data : Maybe MentalMap.Model
    }


initFile : String -> Maybe MentalMap.Model -> File
initFile filename data =
    File filename False False data


type alias Menu r =
    { r
        | files : Array File
        , selection : FileId
        , mdl : Material.Model
    }


addFile : String -> Maybe MentalMap.Model -> Menu r -> Menu r
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
        |> Maybe.andThen .data


setSelected : MentalMap.Model -> Menu r -> Menu r
setSelected mentalMap menu =
    mapFile menu.selection (\old -> { old | data = Just mentalMap }) menu


changeSelection : FileId -> Menu r -> Menu r
changeSelection newId menu =
    if newId >= 0 && newId < Array.length menu.files then
        { menu | selection = newId }
    else
        Debug.crash "Invalid ID!"


init : ( Model, Cmd Msg )
init =
    let
        ( mentalMap, mentalMapCmd ) =
            MentalMap.exampleInit
    in
        Model Material.model
            (Util.Size 0 0)
            -- begin with a test file and mark it as current
            (Just mentalMap |> initFile "New File" |> Array.repeat 5)
            0
            ! [ Task.perform Resize Window.size
              , Cmd.map SelectedMapMsg mentalMapCmd

              {- Send the Cmd _only_ to the selected MentalMap even though each one needs it
                 This is a temporary solution until the Cmd situation gets sorted out
                 It doesn't matter anyway since MentalMap doesn't have a need for Cmd as of right now.
              -}
              ]



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
            let
                ( mentalMap, cmd ) =
                    MentalMap.emptyInit
            in
                addFile "New File" (Just mentalMap) model
                    ! [{- ignore mental map init cmd for now -}]

        MouseOverFile id ->
            mapFile id (\file -> { file | mouseOver = True }) model ! []

        MouseOutFile id ->
            mapFile id (\file -> { file | mouseOver = False }) model ! []



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
                    Html.text "[PLACEHOLDER - no files loaded!]"
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
        , Options.div [ MyCss.mdlClass MyCss.MenuButtons ]
            [ Button.render MdlMsg
                [ 1 ]
                menu.mdl
                [ Button.fab
                , Button.colored
                , Button.ripple
                , Options.onClick NewFile
                ]
                [ Icon.i "add" ]
            ]
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
                List.map
                    (\( msg, iconName ) ->
                        Button.render MdlMsg
                            [ 0, 1, id ]
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
