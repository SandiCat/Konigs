module Main exposing (..)

import MouseManipulator
import Html


main =
    Html.program
        { init = MouseManipulator.init
        , update = MouseManipulator.update
        , subscriptions = MouseManipulator.subscriptions
        , view = MouseManipulator.view
        }
