module GraphMap exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Events as SvgEvents
import TypedSvg.Types as SvgTypes
import TypedSvg.Core as SvgCore exposing (Svg)
import Graph exposing (Graph)
import IntDict
import Node
import Json.Decode
import Color
import Layout
import AnimationFrame
import Time
import Focus exposing ((=>))
import Util.Cmd
import Html
import Html.Events as Events
import Util.Css
import MyCss
import Css
import Util.Misc
import Math.Vector2 as Vec2 exposing (Vec2)
import Edge
import Util.Graph exposing (EdgeId)
import Mouse


-- MODEL


type alias Model =
    { graph : Graph Node.Model Edge.Model
    , state : State
    , mousePos : Vec2
    , cameraPos : Vec2
    }


type State
    = None
    | Connecting Graph.NodeId
    | MovingCamera Vec2 Vec2


type alias UndirectedEdgeId =
    ( Graph.NodeId, Graph.NodeId )


toDirectedEdgeId : UndirectedEdgeId -> Util.Graph.EdgeId
toDirectedEdgeId ( a, b ) =
    -- Edges have random directions in the data structure. Refer to documentation.
    -- However, this function will always give the same direction.
    Util.Graph.EdgeId a b


toUndirectedEdgeId : Graph.NodeId -> Graph.NodeId -> UndirectedEdgeId
toUndirectedEdgeId from to =
    ( from, to )


init : ( Model, Cmd Msg )
init =
    let
        range =
            List.range 0 5

        ( nodes, nodeCmds ) =
            List.map
                (\i ->
                    Vec2.vec2 (toFloat <| 500 + 30 * i) (toFloat <| 300 + (-1) ^ i * 30 * i)
                        |> Node.termNode i ("Test node " ++ toString i)
                        |> Tuple.mapFirst (Graph.Node i)
                        |> Tuple.mapSecond (NodeMsg i |> Cmd.map)
                )
                range
                |> List.unzip

        ( edges, edgeCmds ) =
            [ ( 0, 1 ), ( 0, 2 ), ( 2, 3 ), ( 3, 4 ), ( 2, 5 ), ( 2, 4 ) ]
                |> List.map
                    (\id ->
                        let
                            { from, to } =
                                toDirectedEdgeId id
                        in
                            Tuple.mapFirst (Graph.Edge from to) Edge.init
                                |> Tuple.mapSecond (EdgeMsg id |> Cmd.map)
                    )
                |> List.unzip
    in
        { graph = Graph.fromNodesAndEdges nodes edges
        , state = None
        , mousePos = Vec2.vec2 0 0
        , cameraPos = Vec2.vec2 0 0
        }
            ! (edgeCmds ++ nodeCmds)


getNodePos : Graph.NodeId -> Graph Node.Model e -> Vec2
getNodePos id graph =
    case Graph.get id graph of
        Just { node, incoming, outgoing } ->
            node.label.pos

        Nothing ->
            Vec2.vec2 0 0
                |> Debug.log "getNodePos got nonexisting id!"


offsetMouse : Model -> Vec2
offsetMouse model =
    Vec2.sub model.mousePos model.cameraPos



-- UPDATE


type Msg
    = NoOp
    | StepLayout Time.Time
    | NodeMsg Graph.NodeId Node.Msg
    | EdgeMsg UndirectedEdgeId Edge.Msg
    | Doubleclick
    | Hold
    | Release
    | Move Vec2
    | LeaveWindow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        StepLayout dt ->
            { model | graph = Layout.stepLayout model.graph } ! []

        NodeMsg id nodeMsg ->
            let
                ( maybeNode, cmd, nodeOutMsg ) =
                    case Graph.get id model.graph of
                        Nothing ->
                            ( Nothing, Cmd.none, Nothing )

                        Just ctx ->
                            let
                                ( node, cmd, outMsg ) =
                                    Node.update nodeMsg ctx.node.label
                            in
                                ( Just node, cmd, outMsg )

                focusUpdate ctx node =
                    Focus.update
                        (Util.Misc.nodeFocus => Util.Misc.labelFocus)
                        (always node)
                        ctx

                updateCtx maybeCtx =
                    Maybe.map2 focusUpdate maybeCtx maybeNode

                model2 =
                    { model | graph = Graph.update id updateCtx model.graph }

                ( model3, outMsgCmd ) =
                    updateNodeOutMsg id nodeOutMsg model2
            in
                model3 ! [ Cmd.map (NodeMsg id) cmd, outMsgCmd ]

        EdgeMsg id edgeMsg ->
            case
                Util.Graph.getEdge (toDirectedEdgeId id) model.graph
                    |> Maybe.map (Edge.update edgeMsg)
            of
                Just ( edge, edgeCmd ) ->
                    let
                        { from, to } =
                            toDirectedEdgeId id

                        updateCtx ctx =
                            { ctx | outgoing = IntDict.update to (always <| Just edge) ctx.outgoing }
                    in
                        { model | graph = Graph.update from (Maybe.map updateCtx) model.graph }
                            ! [ Cmd.map (EdgeMsg id) edgeCmd ]

                Nothing ->
                    model ! []

        Doubleclick ->
            let
                id =
                    Util.Graph.newNodeId model.graph

                ( node, nodeCmd ) =
                    Node.termNode id "" <| offsetMouse model
            in
                { model | graph = Util.Graph.addUnconnectedNode id node model.graph }
                    ! [ nodeCmd |> Cmd.map (NodeMsg id) ]

        Hold ->
            case model.state of
                None ->
                    { model | state = MovingCamera model.mousePos model.cameraPos } ! []

                _ ->
                    model ! []

        Release ->
            { model | state = None } ! []

        Move newMousePos ->
            case model.state of
                MovingCamera mousePos cameraPos ->
                    { model | cameraPos = Vec2.add cameraPos (Vec2.sub newMousePos mousePos) } ! []

                _ ->
                    { model | mousePos = newMousePos } ! []

        LeaveWindow ->
            { model | state = None } ! []


updateNodeOutMsg : Graph.NodeId -> Maybe Node.OutMsg -> Model -> ( Model, Cmd Msg )
updateNodeOutMsg id msg model =
    case msg of
        Just Node.MouseDown ->
            { model | state = Connecting id } ! []

        Just Node.MouseUp ->
            case model.state of
                Connecting id_ ->
                    let
                        ( edge, edgeCmd ) =
                            Edge.init

                        edgeId =
                            ( id, id_ )
                    in
                        { model
                            | graph = Util.Graph.addEdge (toDirectedEdgeId edgeId) edge model.graph
                            , state = None
                        }
                            ! [ edgeCmd |> Cmd.map (EdgeMsg edgeId) ]

                _ ->
                    model ! []

        Just Node.Remove ->
            { model | graph = Graph.remove id model.graph } ! []

        Nothing ->
            model ! []



-- VIEW


view : Util.Misc.Size -> Model -> Html.Html Msg
view size model =
    let
        singleEdge view { from, to, label } =
            view (getNodePos from model.graph) (getNodePos to model.graph) label
                |> Html.map (EdgeMsg <| toUndirectedEdgeId from to)

        edges view =
            Graph.edges model.graph
                |> List.map (singleEdge view)

        connectEdge =
            case model.state of
                Connecting id ->
                    [ Edge.svgView model.mousePos (getNodePos id model.graph) connectEdgeModel
                        -- the connecting edge doesn't need to handle messages
                        |> Html.map (always NoOp)
                    ]

                _ ->
                    []

        nodes view =
            Graph.nodes model.graph
                |> List.map (\{ id, label } -> view label |> Html.map (NodeMsg id))
    in
        Util.Css.layers 0
            [ Util.Css.userSelect False
            , Events.onMouseLeave LeaveWindow
            ]
            [ Svg.svg
                [ onDoubleClick Doubleclick
                , Util.Css.userSelect True
                , SvgAttPx.width <| toFloat size.width
                , SvgAttPx.height <| toFloat size.height
                , SvgEvents.onMouseUp Release
                , SvgEvents.onMouseDown Hold
                ]
                [ Svg.g
                    [ SvgAtt.transform
                        [ SvgTypes.Translate
                            (Vec2.getX model.cameraPos)
                            (Vec2.getY model.cameraPos)
                        ]
                    ]
                    (edges Edge.svgView ++ connectEdge ++ nodes Node.svgView)
                ]
            , Html.div
                [ MyCss.class [ MyCss.Nodes ]
                , Util.Css.style
                    [ Vec2.getX model.cameraPos |> Css.px |> Css.left
                    , Vec2.getY model.cameraPos |> Css.px |> Css.top
                    ]
                ]
                (edges Edge.view ++ nodes Node.view)
            ]


connectEdgeModel : Edge.Model
connectEdgeModel =
    let
        ( model, _ ) =
            Edge.init
    in
        model


onDoubleClick : Msg -> SvgCore.Attribute Msg
onDoubleClick msg =
    SvgEvents.on "dblclick" <| Json.Decode.succeed msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Mouse.moves (\{ x, y } -> Move <| Vec2.vec2 (toFloat x) (toFloat y))
            :: AnimationFrame.diffs StepLayout
            :: List.map
                (\{ id, label } -> Sub.map (NodeMsg id) (Node.subscriptions label))
                (Graph.nodes model.graph)
        )
