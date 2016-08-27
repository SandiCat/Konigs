module GraphMap exposing (..)

import Graph
import IntDict
import Node
import Svg
import Svg.Attributes as Att
import SvgUtil
import Layout
import AnimationFrame
import Time
import Focus exposing ((=>))
import CmdUtil
import Html.App
import Html
import CssUtil
import MyCss


-- MODEL

type alias Model =
    { graph: Graph
    }

type alias Graph =
    Graph.Graph Node.Model Edge

type alias Edge = {}

init: (Model, Cmd Msg)
init =
    let
        range = [0..5]

        (nodes, nodeCmds) =
            List.map
                (\i ->
                    Node.testNode (500 + 30*i, 300 + (-1)^i*30*i)
                )
                range
            |> List.unzip

        edges = 
            [ (0, 1)
            , (0, 2)
            , (2, 3)
            , (3, 4)
            , (2, 5)
            , (2, 4)
            ]

        model =
            Graph.fromNodeLabelsAndEdgePairs
                nodes
                (edges ++ (List.map (\(a, b) -> (b, a)) edges)) -- undirected graph
            |> Graph.mapEdges (always {})
            |> Model

        cmds =
            List.map2 (\i cmd -> Cmd.map (NodeMsg i) cmd) range nodeCmds
            |> Cmd.batch
    in
        (model, cmds)

empty: (Model, Cmd Msg)
empty =
    Model Graph.empty |> CmdUtil.noCmd

getNodePos: Graph.NodeId -> Graph -> (Int, Int)
getNodePos id graph =
    case Graph.get id graph of
        Just {node, incoming, outgoing} ->
            node.label.pos
        Nothing -> Debug.log "getNodePos got nonexisting id!" (0, 0)

addEdge: Graph.NodeId -> Graph.NodeId -> Edge -> Graph -> Graph
addEdge a b edge graph =
    let
        exists =
            case Graph.get a graph of
                Just ctx ->
                    IntDict.member b ctx.incoming
                Nothing -> False

        contextUpdate id ctx =
            { ctx
                | incoming = IntDict.insert id edge ctx.incoming
                , outgoing = IntDict.insert id edge ctx.outgoing }
    in
        if a /= b && not exists then
            Graph.update a (contextUpdate b |> Maybe.map) graph
        else
            graph

addUnconnectedNode: Node.Model -> Graph -> (Graph, Graph.NodeId)
addUnconnectedNode node graph =
    let
        id =
            case Graph.nodeIdRange graph of
                Just (a, b) -> b + 1
                Nothing -> 1

        newNode =
            {node = Graph.Node id node, incoming = IntDict.empty, outgoing = IntDict.empty}
    in
        (Graph.insert newNode graph, id)


-- UPDATE

type Msg
    = AddNode (Node.Model, Cmd Node.Msg)
    | AddEdge Graph.NodeId Graph.NodeId Edge
    | StepLayout Time.Time
    | NodeMsg Graph.NodeId Node.Msg

type OutMsg -- there's no ToParent like in Node, it's not needed
    = MouseUp Graph.NodeId
    | MouseDown Graph.NodeId

update: Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update msg model =
    case msg of
        AddNode (node, cmd) ->
            let
                (graph, id) =
                    addUnconnectedNode node model.graph
            in
                ( { model | graph = graph }
                , Cmd.map (NodeMsg id) cmd
                , Nothing
                )
        AddEdge a b edge ->
            ( {model | graph = addEdge a b edge model.graph}
            , Cmd.none
            , Nothing
            )
        StepLayout dt ->
            ({model | graph = Layout.stepLayout model.graph}
            , Cmd.none
            , Nothing
            )
        NodeMsg id nodeMsg ->
            let
                (maybeNode, cmd, nodeOutMsg) =
                    case Graph.get id model.graph of
                        Nothing ->
                            (Nothing, Cmd.none, Nothing)
                        Just ctx ->
                            let
                                (node, cmd, outMsg) = Node.update nodeMsg ctx.node.label
                            in
                                (Just node, cmd, outMsg)

                focusUpdate ctx node =
                    Focus.update
                        (Graph.node => Graph.label)
                        (always node)
                        ctx

                updateCtx maybeCtx =
                    Maybe.map2 focusUpdate maybeCtx maybeNode

                (model', outMsgCmd, outMsg) =
                    updateNodeOutMsg id nodeOutMsg model
            in
                ( {model | graph = Graph.update id updateCtx model.graph}
                , Cmd.batch [ Cmd.map (NodeMsg id) cmd, outMsgCmd ]
                , outMsg
                )

updateNodeOutMsg: Graph.NodeId -> Maybe Node.OutMsg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
updateNodeOutMsg id msg model =
    case msg of
        Just Node.MouseDown -> 
            ( model, Cmd.none, MouseDown id |> Just)
        Just Node.MouseUp -> 
            ( model, Cmd.none, MouseUp id |> Just)
        Nothing ->
            ( model, Cmd.none, Nothing )


-- VIEW

view:
    {width: Int, height: Int} ->
    {xo: Int, yo: Int} ->
    Maybe {mousePos: (Int, Int), originNode: Graph.NodeId} -> 
    Model -> 
    Html.Html Msg
view size camera maybeConnectEdge {graph} =
    let
        toEdgeForm {from, to, label} = edgeView (getNodePos from graph) (getNodePos to graph)

        edges =
            Graph.edges graph
            |> List.map toEdgeForm

        nodes =
            Graph.nodes graph
            |> List.map (\{id, label} -> Node.baseView label |> Html.App.map (NodeMsg id))

        connectEdge =
            case maybeConnectEdge of
                Just {mousePos, originNode} ->
                    [ edgeView mousePos (getNodePos originNode graph) ]
                Nothing -> []
    in
        CssUtil.layers 0 [] 
            [ Svg.svg
                [ toString size.width |> Att.width
                , toString size.height |> Att.height
                ]
                [ Svg.g [ SvgUtil.translate camera.xo camera.yo ]
                    (edges ++ connectEdge ++ nodes)
                ]
            , Html.div 
                [ MyCss.class [ MyCss.NodeCont ]
                , CssUtil.position (camera.xo, camera.yo)
                ]
                ( Graph.nodes graph
                    |> List.map (\{id, label} -> Node.view label |> Html.App.map (NodeMsg id))
                )
            ]

edgeView: (Int, Int) -> (Int, Int) -> Svg.Svg msg
edgeView =
    SvgUtil.line 5 "#244F9F"


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs StepLayout 