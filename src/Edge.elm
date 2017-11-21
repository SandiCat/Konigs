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
import Array exposing (Array)
import Util


-- MODEL


type alias Model =
    { mdl : Material.Model
    , mouseOver : Array Bool

    {- Both the Edge content (the buttons that appear) and the whole edge (the SVG background)
       have to listen for mouse over/out events and if either of them is mouseovered, the
       content has to be displayed. Hence an array (length 2) with one Bool for the content
       and one for the line, keeping the state of "mouse-overing".
    -}
    }


init : ( Model, Cmd Msg )
init =
    Model Material.model (Array.repeat 2 False) ! []



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | NoOp
    | MouseOver Int Bool -- id (index) of which state to change and what to change it to
    | ToParent OutMsg


type OutMsg
    = Remove


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            let
                ( model_, cmd ) =
                    Material.update MdlMsg msg_ model
            in
                ( model_, cmd, Nothing )

        NoOp ->
            ( model, Cmd.none, Nothing )

        MouseOver id newState ->
            ( { model | mouseOver = Array.set id newState model.mouseOver }
            , Cmd.none
            , Nothing
            )

        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )



-- VIEW


view : Vec2 -> Vec2 -> Model -> Html.Html Msg
view pos1 pos2 model =
    if Array.foldl (||) False model.mouseOver then
        let
            ( x, y ) =
                -- the middle of the edge
                Vec2.add pos1 pos2
                    |> Vec2.scale 0.5
                    |> Vec2.toTuple
        in
            Html.div
                [ MyCss.class [ MyCss.Edge ]
                , SvgEvents.onMouseOver (MouseOver 1 True)
                , SvgEvents.onMouseOut (MouseOver 1 False)
                ]
                [ Button.render
                    MdlMsg
                    [ 0 ]
                    model.mdl
                    [ Button.icon
                    , Options.onClick NoOp
                    ]
                    [ Icon.i "clear" ]
                ]
                |> List.singleton
                |> Html.div
                    [ MyCss.class [ MyCss.EdgeCont ]
                    , Util.Css.style
                        [ x |> Css.px |> Css.left
                        , y |> Css.px |> Css.top
                        ]
                    ]
    else
        Html.div [] []


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
            , SvgEvents.onMouseOver (MouseOver 0 True)
            , SvgEvents.onMouseOut (MouseOver 0 False)
            ]
            []
        ]
