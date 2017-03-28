port module Stylesheets exposing (..)

import Css.File
import MyCss
import Html exposing (div)


port files : Css.File.CssFileStructure -> Cmd msg


fileStructure : Css.File.CssFileStructure
fileStructure =
    Css.File.toFileStructure [ ( "mainstyle.css", Css.File.compile [ MyCss.css ] ) ]


main : Css.File.CssCompilerProgram
main =
    Css.File.compiler files fileStructure
