import Time
import Signal
import Mouse
import Window
import Html
import MouseManipulator exposing (Action (..))
import Mouse


-- this could have been prettier if there sampleOn kept values from both signals
main: Signal.Signal Html.Html
main =
    Signal.merge
        ( Signal.sampleOn Mouse.isDown Mouse.position
            |> Signal.map2
                (\state pos -> (if state then Hold else Release, pos))
                Mouse.isDown
            |> Time.timestamp
            |> Signal.map2
                (\(w, h) (t, (state, (x, y))) -> state (x, y) t)
                Window.dimensions
        )
        ( Mouse.position
            |> Signal.map Move
        )
    |> Signal.foldp MouseManipulator.update MouseManipulator.testModel
    |> Signal.map2 MouseManipulator.view Window.dimensions
