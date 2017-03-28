port module Stylesheets exposing (..)

import Css.File exposing (..)
import MyCss
import Html exposing (div)


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "mainstyle.css", compile [ MyCss.css ] ) ]


main =
    Platform.program
        { init = ( (), files cssFiles )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
