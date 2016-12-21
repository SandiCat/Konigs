module Option exposing (..)

import Color
import Svg
import Html.App

type alias Option msg =
    { msg: msg
    , icon: Svg.Svg msg
    }

iconSize: Int
iconSize = 30

color: Color.Color
color = Color.rgb 19 56 125

map: (a -> b) -> Option a -> Option b
map f old =
    Option (f old.msg) (Html.App.map f old.icon)