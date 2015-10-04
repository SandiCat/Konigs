import Time
import Signal
import Mouse
import Window
import Html
import Html.Lazy
import MouseManipulator exposing (Action (..))
import Mouse


main: Signal.Signal Html.Html
main =
    Signal.mergeMany
        [ Signal.sampleOn Mouse.isDown Mouse.position
            -- this could have been prettier if sampleOn kept values from both signals
            |> Signal.map2
                (\state pos -> (if state then Hold else Release, pos))
                Mouse.isDown
            |> Time.timestamp
            |> Signal.map2
                (\(w, h) (t, (state, (x, y))) -> state (x, y) t)
                Window.dimensions
        , Mouse.position
            |> Signal.map Move
        , Time.fps 30
            |> Signal.map Tick
        ]
    |> Signal.foldp MouseManipulator.update MouseManipulator.testModel
    |> Signal.map2 (Html.Lazy.lazy2 MouseManipulator.view) Window.dimensions