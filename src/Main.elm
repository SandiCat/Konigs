module Main exposing (..)

import MouseManipulator


main : Program Never
main =
    Html.program
        { init = MouseManipulator.init
        , update = MouseManipulator.update
        , subscriptions = MouseManipulator.subscriptions
        , view = MouseManipulator.view
        }
