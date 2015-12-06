import Time
import Signal
import Mouse
import Window
import Html
import MouseManipulator exposing (Action (..))
import Mouse


mailbox: Signal.Mailbox MouseManipulator.Action
mailbox =
    Signal.mailbox (Tick 0)

main: Signal.Signal Html.Html
main =
    Signal.mergeMany
        [ Signal.map2
            (\s (x, y) -> (if s then Hold else Release) (x, y))
            Mouse.isDown Mouse.position
            |> Signal.sampleOn Mouse.isDown
        , Mouse.position
            |> Signal.map Move
        , Time.fps 30
            |> Signal.map Tick
        , mailbox.signal
        ]
    |> Signal.foldp MouseManipulator.update MouseManipulator.testModel
    |> Signal.map2 (MouseManipulator.view mailbox.address) Window.dimensions