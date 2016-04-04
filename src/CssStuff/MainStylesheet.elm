module CssStuff.MainStylesheet (css) where

import Css exposing (..)
import Css.Elements exposing (..)
import SharedStyles exposing (..)


css: Css.Stylesheet
css =
    --(stylesheet << mainNamespace)
    stylesheet
        [ ((.) Term)
            [ color (rgb 255 0 0) ]
        , ((#) MouseManipulator)
            []
        ]