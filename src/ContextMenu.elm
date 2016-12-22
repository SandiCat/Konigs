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
import MetaContent
import Option exposing (Option)
import Html.App


-- MODEL

type alias Model =
    { mouseOver: Bool
    }

init: Model
init =
    Model False


-- UPDATE

type Msg
    = ToParent OutMsg
    | MouseOver
    | MouseOut

type OutMsg
    = Remove
    | Edit
    | ContentMsg MetaContent.MultiMsg

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

baseOptions: List (Option OutMsg)
baseOptions =
    [ delete Option.color Option.iconSize |> Option Remove
    , edit Option.color Option.iconSize |> Option Edit
    ]

view: List (Option MetaContent.MultiMsg) -> Model -> Html.Html Msg
view options model =
    let
        options' =
            List.map (Option.map ContentMsg) options
    in
        List.map optionView (options' ++ baseOptions)
        |> Html.div
            [ MyCss.class [ MyCss.ContextMenu ]
            , Events.onMouseOver MouseOver
            , Events.onMouseOut MouseOut
            ]

optionView: Option OutMsg -> Html.Html Msg
optionView option =
    Svg.svg
        (SvgUtil.size Option.iconSize Option.iconSize)
        [ option.icon |> Html.App.map ToParent ]
    |> List.Extra.singleton
    |> Html.div
        [ MyCss.class [ MyCss.MenuIcon ]
        , ToParent option.msg |> Events.onClick
        ]