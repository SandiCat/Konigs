module Edge exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes
import TypedSvg.Events as SvgEvents
import Color
import Html.Events as Events
import Html
import MyCss
import Util.Css
import Css
import Platform.Cmd as Cmd
import Util.Cmd
import Math.Vector2 as Vec2 exposing (Vec2)
import Material.Button as Button
import Material.Icon as Icon
import Material
import Material.Options as Options


-- MODEL


type alias Model =
    { mdl : Material.Model
    , mouseOver : Bool
    }


init : ( Model, Cmd Msg )
init =
    Model Material.model False ! []



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | NoOp
    | MouseOver
    | MouseOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        NoOp ->
            model
                ! []
                |> Debug.log "x click"

        MouseOver ->
            { model | mouseOver = True } ! []

        MouseOut ->
            { model | mouseOver = False } ! []



-- VIEW


view : Vec2 -> Vec2 -> Model -> Html.Html Msg
view pos1 pos2 model =
    let
        ( x1, y1 ) =
            Vec2.toTuple pos1

        ( x2, y2 ) =
            Vec2.toTuple pos2

        ( x, y ) =
            -- top left corner
            [ ( x1, y1 ), ( x2, y2 ), ( x1, y2 ), ( x2, y1 ) ]
                |> List.sortBy Tuple.first
                |> List.sortBy Tuple.second
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
        Html.div
            [ MyCss.class [ MyCss.Edge ]
            , Util.Css.style
                [ x |> Css.px |> Css.left
                , y |> Css.px |> Css.top
                , Vec2.sub pos1 pos2 |> Vec2.getX |> abs |> Css.px |> Css.width
                , Vec2.sub pos1 pos2 |> Vec2.getY |> abs |> Css.px |> Css.height
                ]
            ]
            [ if model.mouseOver then
                Html.div [ MyCss.class [ MyCss.EdgeContent ] ]
                    [ Button.render
                        MdlMsg
                        [ 0 ]
                        model.mdl
                        [ Button.icon
                        , Options.onClick NoOp
                        ]
                        [ Icon.i "clear" ]
                    ]
              else
                Html.div [] []
            ]


svgView : Vec2 -> Vec2 -> Model -> Svg Msg
svgView pos1 pos2 model =
    Svg.g []
        [ Svg.line
            [ SvgAttPx.x1 <| Vec2.getX pos1
            , SvgAttPx.y1 <| Vec2.getY pos1
            , SvgAttPx.x2 <| Vec2.getX pos2
            , SvgAttPx.y2 <| Vec2.getY pos2
            , SvgAtt.stroke <| Color.rgb 36 79 159
            , SvgAttPx.strokeWidth 5
            ]
            []
        , Svg.line
            [ SvgAttPx.x1 <| Vec2.getX pos1
            , SvgAttPx.y1 <| Vec2.getY pos1
            , SvgAttPx.x2 <| Vec2.getX pos2
            , SvgAttPx.y2 <| Vec2.getY pos2
            , SvgAtt.stroke Color.black
            , SvgAtt.strokeOpacity (SvgTypes.Opacity 0)
            , SvgAttPx.strokeWidth 40
            , SvgEvents.onMouseOver MouseOver
            , SvgEvents.onMouseOut MouseOut
            ]
            []
        ]
