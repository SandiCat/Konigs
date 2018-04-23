module Edge exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes
import TypedSvg.Events as SvgEvents
import Color
import Html.Events as Events
import Html.Attributes
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
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode


-- MODEL


type alias Model =
    { mdl : Material.Model
    , mouseOver : Array Bool

    {- Both the Edge content (the buttons that appear) and the whole edge (the SVG background)
       have to listen for mouse over/out events and if either of them is mouseovered, the
       content has to be displayed. Hence an array (length 2) with one Bool for the content
       and one for the line, keeping the state of "mouse-overing".
    -}
    , direction : Direction
    }


type Direction
    = Undirected
    | FromTo
    | ToFrom


init : Direction -> Model
init =
    Model Material.model (Array.repeat 2 False)



-- JSON


decodeDirection : Decode.Decoder Direction
decodeDirection =
    Decode.string
        |> Decode.map
            (\s ->
                case s of
                    "Undirected" ->
                        Undirected

                    "FromTo" ->
                        FromTo

                    "ToFrom" ->
                        ToFrom

                    _ ->
                        Util.crashLog "Unknown direction encoding" Undirected
            )


decode : Decode.Decoder Model
decode =
    Decode.succeed init
        |: Decode.field "direction" decodeDirection


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "direction", toString model.direction |> Encode.string ) ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | NoOp
    | MouseOver Int Bool -- id (index) of which state to change and what to change it to
    | ToParent OutMsg
    | ChangeDirection Direction


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

        ChangeDirection direction ->
            ( { model | direction = direction }, Cmd.none, Nothing )



-- VIEW
{- These functions expect to be passed the two coordinates in a consitent order.
   As is described in the docs, the direction is determined by a value in the Edge's
   Model, but the graph structure has direction, too. The structure direction is what the
   'from' and 'to' refer to.
-}


view : Vec2 -> Vec2 -> Model -> Html.Html Msg
view from to model =
    if Array.foldl (||) False model.mouseOver then
        let
            ( vx, vy ) =
                Vec2.sub to from |> Vec2.toTuple

            angle =
                -- negative y because of different coordinate systems
                -- negate because css rotates clockwise
                atan2 (-vy) vx |> negate

            midPoint =
                Vec2.add from to |> Vec2.scale 0.5

            -- Distance from edge to the origin of the buttons
            distance =
                30

            -- A point just above/below the middle of the edge
            { x, y } =
                Vec2.direction to from
                    |> Util.rotateVec2 (pi / 2)
                    |> Vec2.scale distance
                    |> Vec2.add midPoint
                    |> Vec2.toRecord
        in
            Html.div
                [ MyCss.class [ MyCss.Edge ]
                , SvgEvents.onMouseOver (MouseOver 1 True)
                , SvgEvents.onMouseOut (MouseOver 1 False)
                , Util.Css.style
                    [ Css.transforms
                        [ Css.translate2 (Css.pct -50) (Css.pct -50)
                        , Css.rotateZ <| Css.rad angle
                        ]
                    , Css.px x |> Css.left
                    , Css.px y |> Css.top
                    ]
                ]
                (List.indexedMap
                    (\i ( msg, iconName, description ) ->
                        Button.render MdlMsg
                            [ 0, 0, i ]
                            model.mdl
                            [ Button.icon
                            , Options.onClick msg
                            , Options.attribute <| Html.Attributes.title description
                            ]
                            [ Icon.i iconName ]
                    )
                    [ ( ChangeDirection ToFrom, "arrow_back", "Change direction" )
                    , ( ChangeDirection Undirected, "swap_horiz", "Remove direction" )
                    , ( ToParent Remove, "clear", "Delete" )
                    , ( ChangeDirection FromTo, "arrow_forward", "Change direction" )
                    ]
                )
    else
        Html.div [] []


edgeColor =
    Color.rgb 36 79 159


edgeWidth =
    5


svgDefs =
    Svg.defs []
        [ Svg.marker
            [ Html.Attributes.attribute "markerWidth" "2"
            , Html.Attributes.attribute "markerHeight" "1"
            , Html.Attributes.attribute "preserveAspectRatio" "none"
            , SvgAtt.viewBox 0 0 2 2
            , SvgAtt.refX "0"
            , SvgAtt.refY "1"
            , Html.Attributes.id "arrow"
            , SvgAtt.orient "auto"
            , SvgAtt.markerUnits (SvgTypes.MarkerCoordinateSystemStrokeWidth)
            ]
            [ Svg.path
                [ SvgAtt.d "M 0 0 L 1 0 L 2 1 L 1 2 L 0 2 L 1 1 Z"
                , SvgAtt.fill <| SvgTypes.Fill <| Color.rgb 94 129 193
                ]
                []
            ]
        ]


svgView : Vec2 -> Vec2 -> Model -> Svg Msg
svgView from to model =
    Svg.g []
        [ case model.direction of
            Undirected ->
                Svg.line
                    [ SvgAttPx.x1 <| Vec2.getX from
                    , SvgAttPx.y1 <| Vec2.getY from
                    , SvgAttPx.x2 <| Vec2.getX to
                    , SvgAttPx.y2 <| Vec2.getY to
                    , SvgAttPx.strokeWidth edgeWidth
                    , SvgAtt.stroke edgeColor
                    ]
                    []

            FromTo ->
                directedLine from to

            ToFrom ->
                directedLine to from
        , Svg.line
            [ SvgAttPx.x1 <| Vec2.getX from
            , SvgAttPx.y1 <| Vec2.getY from
            , SvgAttPx.x2 <| Vec2.getX to
            , SvgAttPx.y2 <| Vec2.getY to
            , SvgAtt.stroke Color.black
            , SvgAtt.strokeOpacity (SvgTypes.Opacity 0)
            , SvgAttPx.strokeWidth 40
            , SvgEvents.onMouseOver (MouseOver 0 True)
            , SvgEvents.onMouseOut (MouseOver 0 False)
            ]
            []
        ]


directedLine : Vec2 -> Vec2 -> Svg Msg
directedLine from to =
    let
        spacing =
            5

        vec =
            Vec2.sub to from

        ( dx, dy ) =
            Vec2.scale (spacing / Vec2.length vec) vec
                |> Vec2.toTuple

        ( x0, y0 ) =
            Vec2.toTuple from
    in
        Svg.g []
            [ Svg.line
                [ SvgAttPx.x1 <| Vec2.getX from
                , SvgAttPx.y1 <| Vec2.getY from
                , SvgAttPx.x2 <| Vec2.getX to
                , SvgAttPx.y2 <| Vec2.getY to
                , SvgAttPx.strokeWidth edgeWidth
                , SvgAtt.stroke edgeColor
                ]
                []
            , List.map
                (toFloat
                    >> (\i ->
                            Svg.line
                                [ SvgAttPx.x1 <| x0 + 2 * i * dx
                                , SvgAttPx.y1 <| y0 + 2 * i * dy
                                , SvgAttPx.x2 <| x0 + (2 * i + 1) * dx
                                , SvgAttPx.y2 <| y0 + (2 * i + 1) * dy
                                , SvgAtt.strokeOpacity (SvgTypes.Opacity 0)
                                , SvgAttPx.strokeWidth edgeWidth
                                , SvgAtt.markerEnd "url(#arrow)"
                                ]
                                []
                       )
                )
                (List.range 0 <| truncate <| Vec2.length vec / spacing / 2)
                |> Svg.g []
            ]
