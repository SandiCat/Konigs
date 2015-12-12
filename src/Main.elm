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
        [ Mouse.position
            |> Signal.map Move
        , Time.fps 30
            |> Signal.map Tick
        , mailbox.signal
        ]
    |> Signal.foldp MouseManipulator.update MouseManipulator.testModel
    |> Signal.map2 (MouseManipulator.view mailbox.address) Window.dimensions