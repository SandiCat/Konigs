module GraphMap exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Events as SvgEvents
import TypedSvg.Types as SvgTypes
import TypedSvg.Core as SvgCore exposing (Svg)
import Graph
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


-- MODEL


type alias Model =
    { graph : Graph
    }


type alias Graph =
    Graph.Graph Node.Model Edge


type alias Edge =
    {}


init : ( Model, Cmd Msg )
init =
    let
        range =
            List.range 0 5

        ( nodes, nodeCmds ) =
            List.map
                (\i ->
                    Vec2.vec2 (toFloat <| 500 + 30 * i) (toFloat <| 300 + (-1) ^ i * 30 * i)
                        |> Node.testNode
                )
                range
                |> List.unzip

        edges =
            [ ( 0, 1 )
            , ( 0, 2 )
            , ( 2, 3 )
            , ( 3, 4 )
            , ( 2, 5 )
            , ( 2, 4 )
            ]

        model =
            Graph.fromNodeLabelsAndEdgePairs
                nodes
                (edges ++ (List.map (\( a, b ) -> ( b, a )) edges))
                -- undirected graph
                |> Graph.mapEdges (always {})
                |> Model

        cmds =
            List.map2 (\i cmd -> Cmd.map (NodeMsg i) cmd) range nodeCmds
                |> Cmd.batch
    in
        ( model, cmds )


empty : ( Model, Cmd Msg )
empty =
    Model Graph.empty ! []


getNodePos : Graph.NodeId -> Graph -> Vec2
getNodePos id graph =
    case Graph.get id graph of
        Just { node, incoming, outgoing } ->
            node.label.pos

        Nothing ->
            Vec2.vec2 0 0
                |> Debug.log "getNodePos got nonexisting id!"


addEdge : Graph.NodeId -> Graph.NodeId -> Edge -> Graph -> Graph
addEdge a b edge graph =
    let
        exists =
            case Graph.get a graph of
                Just ctx ->
                    IntDict.member b ctx.incoming

                Nothing ->
                    False

        contextUpdate id ctx =
            { ctx
                | incoming = IntDict.insert id edge ctx.incoming
                , outgoing = IntDict.insert id edge ctx.outgoing
            }
    in
        if a /= b && not exists then
            Graph.update a (contextUpdate b |> Maybe.map) graph
        else
            graph


addUnconnectedNode : Node.Model -> Graph -> ( Graph, Graph.NodeId )
addUnconnectedNode node graph =
    let
        id =
            case Graph.nodeIdRange graph of
                Just ( a, b ) ->
                    b + 1

                Nothing ->
                    1

        newNode =
            { node = Graph.Node id node, incoming = IntDict.empty, outgoing = IntDict.empty }
    in
        ( Graph.insert newNode graph, id )



-- UPDATE


type Msg
    = AddNode ( Node.Model, Cmd Node.Msg )
    | AddEdge Graph.NodeId Graph.NodeId Edge
    | StepLayout Time.Time
    | NodeMsg Graph.NodeId Node.Msg
    | ToParent OutMsg


type OutMsg
    = MouseUp Graph.NodeId
    | MouseDown Graph.NodeId
    | Doubleclick
    | Hold
    | Release


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )

        AddNode ( node, cmd ) ->
            let
                ( graph, id ) =
                    addUnconnectedNode node model.graph
            in
                ( { model | graph = graph }
                , Cmd.map (NodeMsg id) cmd
                , Nothing
                )

        AddEdge a b edge ->
            ( { model | graph = addEdge a b edge model.graph }
            , Cmd.none
            , Nothing
            )

        StepLayout dt ->
            ( { model | graph = Layout.stepLayout model.graph }
            , Cmd.none
            , Nothing
            )

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

                ( model3, outMsgCmd, outMsg ) =
                    updateNodeOutMsg id nodeOutMsg model2
            in
                ( model3
                , Cmd.batch [ Cmd.map (NodeMsg id) cmd, outMsgCmd ]
                , outMsg
                )


updateNodeOutMsg : Graph.NodeId -> Maybe Node.OutMsg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateNodeOutMsg id msg model =
    case msg of
        Just Node.MouseDown ->
            ( model, Cmd.none, MouseDown id |> Just )

        Just Node.MouseUp ->
            ( model, Cmd.none, MouseUp id |> Just )

        Just Node.Remove ->
            ( { model | graph = Graph.remove id model.graph }
            , Cmd.none
            , Nothing
            )

        Nothing ->
            ( model, Cmd.none, Nothing )



-- VIEW


view :
    { width : Int, height : Int }
    -> Vec2
    -> Maybe { mousePos : Vec2, originNode : Graph.NodeId }
    -> Model
    -> Html.Html Msg
view size camera maybeConnectEdge { graph } =
    let
        toEdgeForm { from, to, label } =
            edgeView (getNodePos from graph) (getNodePos to graph)

        edges =
            Graph.edges graph
                |> List.map toEdgeForm

        nodes =
            Graph.nodes graph
                |> List.map (\{ id, label } -> Node.baseView label |> Html.map (NodeMsg id))

        connectEdge =
            case maybeConnectEdge of
                Just { mousePos, originNode } ->
                    [ edgeView mousePos (getNodePos originNode graph) ]

                Nothing ->
                    []
    in
        Util.Css.layers 0
            []
            [ Svg.svg
                [ onDoubleClick (ToParent Doubleclick)
                , Util.Css.userSelect True
                , SvgAttPx.width <| toFloat size.width
                , SvgAttPx.height <| toFloat size.height
                , SvgEvents.onMouseUp (ToParent Release)
                , SvgEvents.onMouseDown (ToParent Hold)
                ]
                [ Svg.g
                    [ SvgAtt.transform [ SvgTypes.Translate (Vec2.getX camera) (Vec2.getY camera) ] ]
                    (edges ++ connectEdge ++ nodes)
                ]
            , Html.div
                [ MyCss.class [ MyCss.Nodes ]
                , Util.Css.style
                    [ Vec2.getX camera |> Css.px |> Css.left
                    , Vec2.getY camera |> Css.px |> Css.top
                    ]
                ]
                (Graph.nodes graph
                    |> List.map (\{ id, label } -> Node.view label |> Html.map (NodeMsg id))
                )
            ]


onDoubleClick : Msg -> SvgCore.Attribute Msg
onDoubleClick msg =
    SvgEvents.on "dblclick" <| Json.Decode.succeed msg


edgeView : Vec2 -> Vec2 -> Svg msg
edgeView pos1 pos2 =
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (AnimationFrame.diffs StepLayout
            :: List.map
                (\{ id, label } -> Sub.map (NodeMsg id) (Node.subscriptions label))
                (Graph.nodes model.graph)
        )
