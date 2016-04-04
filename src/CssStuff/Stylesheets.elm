module CssStuff.Stylesheets (..) where

import Css.File exposing (..)
import MainStylesheet


port files : CssFileStructure
port files =
  toFileStructure
    [ ( "mainstyle.css", compile MainStylesheet.css ) ]
