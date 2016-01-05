import Effects
import MouseManipulator
import StartApp
import Task
import Window
import Mouse
import Html


inputs: List (Signal MouseManipulator.Action)
inputs =
    [ Mouse.position
        |> Signal.map MouseManipulator.Move
    , Window.dimensions
        |> Signal.map MouseManipulator.Resize
    ]

app: StartApp.App MouseManipulator.Model
app =
  StartApp.start
    { init = MouseManipulator.init
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
