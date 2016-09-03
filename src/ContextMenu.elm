module ContextMenu exposing (..)

import Html
import Html.Events as Events
import MyCss
import Svg
import SvgUtil
import Material.Icons.Action exposing (delete)
import Material.Icons.Image exposing (edit)
import Color
import List.Extra


type Msg
    = Remove
    | Edit
    | MouseOver
    | MouseOut

type alias Option =
    { msg: Msg
    , icon: Svg.Svg Msg
    }

iconSize: Int
iconSize = 30

color: Color.Color
color = Color.rgb 19 56 125

options: List Option
options =
    [ delete color iconSize |> Option Remove
    , edit color iconSize |> Option Edit
    ]

view: Html.Html Msg
view =
    List.map optionView options
    |> Html.div
        [ MyCss.class [ MyCss.ContextMenu ]
        , Events.onMouseOver MouseOver
        , Events.onMouseOut MouseOut
        ]


optionView: Option -> Html.Html Msg
optionView option =
    Svg.svg
        (SvgUtil.size iconSize iconSize)
        [ option.icon ]
    |> List.Extra.singleton
    |> Html.div
        [ MyCss.class [ MyCss.MenuIcon ]
        , Events.onClick option.msg
        ]