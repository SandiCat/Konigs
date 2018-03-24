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
import Array exposing (Array)


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
    , data : Maybe MentalMap.Model
    }


type alias Menu r =
    { r
        | files : Array File
        , selection : FileId
    }


getSelected : Menu r -> Maybe MentalMap.Model
getSelected menu =
    Array.get menu.selection menu.files
        |> Maybe.andThen .data


setSelected : MentalMap.Model -> Menu r -> Menu r
setSelected mentalMap menu =
    case Array.get menu.selection menu.files of
        Just old ->
            { menu
                | files =
                    Array.set menu.selection
                        { old | data = Just mentalMap }
                        menu.files
            }

        Nothing ->
            Debug.log "Trying to set nonexistent selection!" menu


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
            MentalMap.init
    in
        Model Material.model
            (Util.Size 0 0)
            -- begin with a test file and mark it as current
            (Just mentalMap |> File "New File" |> Array.repeat 5)
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
    = Resize Util.Size
    | SelectedMapMsg MentalMap.Msg
    | MdlMsg (Material.Msg Msg)
    | ChangeSelection FileId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    Material.List.ul []
        (Array.indexedMap (viewFile menu) menu.files |> Array.toList)


viewFile : Menu r -> FileId -> File -> Html.Html Msg
viewFile menu id { filename, data } =
    Material.List.li
        [ Options.onClick <| ChangeSelection id
        , if menu.selection == id then
            Material.Color.color Material.Color.Red Material.Color.S200
                |> Material.Color.background
          else
            Options.nop
        ]
        [ Html.text filename ]



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
