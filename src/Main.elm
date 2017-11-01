module Main exposing (..)

import Task
import Html
import Util.Misc as Util
import Window
import MouseManipulator
import Util.Cmd


-- MODEL


type alias Model =
    { size : Util.Size
    , mouseManipulator : MouseManipulator.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( mm, mmCmd ) =
            MouseManipulator.init
    in
        Model (Util.Size 0 0) mm
            ! [ Task.perform Resize Window.size
              , mmCmd |> Cmd.map MouseManipulatorMsg
              ]



-- UPDATE


type Msg
    = Resize Util.Size
    | MouseManipulatorMsg MouseManipulator.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

        MouseManipulatorMsg msg_ ->
            Util.Cmd.update
                (\x -> { model | mouseManipulator = x })
                MouseManipulatorMsg
                (MouseManipulator.update msg_ model.mouseManipulator)



-- VIEW


view : Model -> Html.Html Msg
view model =
    MouseManipulator.view model.size model.mouseManipulator
        |> Html.map MouseManipulatorMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , MouseManipulator.subscriptions model.mouseManipulator
            |> Sub.map MouseManipulatorMsg
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
