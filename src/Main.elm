module Main exposing (..)

import MouseManipulator
import Html.App


main : Program Never
main =
    Html.App.program
        { init = MouseManipulator.init
        , update = MouseManipulator.update
        , subscriptions = MouseManipulator.subscriptions
        , view = MouseManipulator.view
        }
