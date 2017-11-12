module Edge exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes
import Color
import Html.Events as Events
import Html
import MyCss
import Util.Css
import Css
import Platform.Cmd as Cmd
import Util.Cmd
import Math.Vector2 as Vec2 exposing (Vec2)


-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    {} ! []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div [] []


svgView : Vec2 -> Vec2 -> Model -> Svg Msg
svgView pos1 pos2 model =
    line pos1 pos2


line : Vec2 -> Vec2 -> Svg a
line pos1 pos2 =
    Svg.line
        [ SvgAttPx.x1 <| Vec2.getX pos1
        , SvgAttPx.y1 <| Vec2.getY pos1
        , SvgAttPx.x2 <| Vec2.getX pos2
        , SvgAttPx.y2 <| Vec2.getY pos2
        , SvgAtt.stroke <| Color.rgb 36 79 159
        , SvgAtt.fillOpacity <| SvgTypes.Opacity 0
        , SvgAttPx.strokeWidth 5
        ]
        []
