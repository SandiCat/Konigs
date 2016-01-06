import Effects exposing (Effects)
import MouseManipulator
import CustomStartApp
import Task
import Window
import Mouse
import Html


inputs: List (Signal MouseManipulator.Action)
inputs =
    [ Window.dimensions
        |> Signal.map MouseManipulator.Resize
    , Mouse.position
        |> Signal.map MouseManipulator.Move
    ]

init: List MouseManipulator.Action -> ( MouseManipulator.Model, Effects MouseManipulator.Action )
init =
    let
        update action (model, fx) =
            let
                (model', fx') = MouseManipulator.update action model
            in
                (model', Effects.batch [fx, fx'])
    in
        List.foldr update MouseManipulator.init

app: CustomStartApp.App MouseManipulator.Model
app =
    CustomStartApp.start
        { init = init
        , update = MouseManipulator.update
        , view = MouseManipulator.view
        , inputs = inputs
        }

main: Signal Html.Html
main =
    app.html

port tasks: Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
