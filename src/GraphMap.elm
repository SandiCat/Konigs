module GraphMap exposing (..)

import Graph
import IntDict
import Node
import Svg
import SvgUtil
import Layout
import AnimationFrame
import Time
import Focus exposing ((=>))
import CmdUtil
import Html.App


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

        (nodes, nodeFxs) =
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

        fxs =
            List.map2 (\i fx -> Cmd.map (NodeMsg i) fx) range nodeFxs
            |> Cmd.batch
    in
        (model, fxs)

empty: (Model, Cmd Msg)
empty =
    Model Graph.empty |> CmdUtil.noFx

getNodePos: Graph.NodeId -> Graph -> Maybe (Int, Int)
getNodePos id graph =
    case Graph.get id graph of
        Just {node, incoming, outgoing} ->
            Just node.label.pos
        Nothing -> Nothing

addEdge: Graph.NodeId -> Graph.NodeId -> Edge -> Graph -> Graph
addEdge a b edge graph =
    let
        exists =
            case Graph.get a graph of
                Just ctx ->
                    IntDict.member b ctx.incoming
                Nothing -> False
        contextUpdate id maybeCtx =
            case maybeCtx of
                Nothing -> Nothing
                Just ctx -> Just
                    {ctx | incoming = IntDict.insert id edge ctx.incoming}
    in
        if a /= b && not exists then
            Graph.update a (contextUpdate b) graph
            |> Graph.update b (contextUpdate a)
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
        AddNode (node, fx) ->
            let
                (graph, id) =
                    addUnconnectedNode node model.graph
            in
                ( { model | graph = graph }
                , Cmd.map (NodeMsg id) fx
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
                (maybeNode, fx, nodeOutMsg) =
                    case Graph.get id model.graph of
                        Nothing ->
                            (Nothing, Cmd.none, Nothing)
                        Just ctx ->
                            let
                                (node, fx, outMsg) = Node.update nodeMsg ctx.node.label
                            in
                                (Just node, fx, outMsg)

                focusUpdate ctx node =
                    Focus.update
                        (Graph.node => Graph.label)
                        (always node)
                        ctx

                updateCtx maybeCtx =
                    Maybe.map2 focusUpdate maybeCtx maybeNode

                (model', outMsgFx, outMsg) =
                    updateNodeOutMsg id nodeOutMsg model
            in
                ( {model | graph = Graph.update id updateCtx model.graph}
                , Cmd.batch [ Cmd.map (NodeMsg id) fx, outMsgFx ]
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
            (model, Cmd.none, Nothing)


-- VIEW

view: Model -> Svg.Svg Msg
view {graph} =
    let
        toEdgeForm {from, to, label} =
            case Maybe.map2 (,) (getNodePos from graph) (getNodePos to graph) of
                Just (a, b) ->
                    edgeForm a b
                Nothing ->
                    Svg.g [] []
        edges =
            Graph.edges graph
            |> List.map toEdgeForm

        nodes =
            Graph.nodes graph
            |> List.map (\{id, label} -> Node.view label |> Html.App.map (NodeMsg id))
    in
        Svg.g [] (edges ++ nodes)

edgeForm: (Int, Int) -> (Int, Int) -> Svg.Svg msg
edgeForm =
    SvgUtil.line 5 "#244F9F"


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs StepLayout 