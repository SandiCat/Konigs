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


-- MODEL

type alias Model =
    { options: List Option
    , mouseOver: Bool
    }

type alias Option =
    { msg: OutMsg
    , icon: Svg.Svg Msg
    }

init: Model
init =
    Model
        [ delete color iconSize |> Option Remove
        , edit color iconSize |> Option Edit
        ]
        False


-- UPDATE

type Msg
    = ToParent OutMsg
    | MouseOver
    | MouseOut

type OutMsg
    = Remove
    | Edit

update: Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update msg model =
    case msg of
        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )
        MouseOver ->
            ( { model | mouseOver = True }, Cmd.none, Nothing )
        MouseOut ->
            ( { model | mouseOver = False }, Cmd.none, Nothing )


-- VIEW

view: Model -> Html.Html Msg
view model =
    List.map optionView model.options
    |> Html.div
        [ MyCss.class [ MyCss.ContextMenu ]
        , Events.onMouseOver MouseOver
        , Events.onMouseOut MouseOut
        ]

iconSize: Int
iconSize = 30

color: Color.Color
color = Color.rgb 19 56 125

optionView: Option -> Html.Html Msg
optionView option =
    Svg.svg
        (SvgUtil.size iconSize iconSize)
        [ option.icon ]
    |> List.Extra.singleton
    |> Html.div
        [ MyCss.class [ MyCss.MenuIcon ]
        , ToParent option.msg |> Events.onClick
        ]