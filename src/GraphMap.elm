module GraphMap where

import Graph
import IntDict
import Node
import Debug
import Svg
import SvgHelp
import List.Extra
import Layout
import Time
import Focus exposing ((=>))
import NodeBase


-- MODEL

type alias Model =
    { graph: Graph
    }

type alias Graph =
    Graph.Graph Node.Model Edge

type alias Edge = {}

testModel: Model
testModel =
    Graph.fromNodeLabelsAndEdgePairs
        (List.map 
            (\i -> 
                Node.testNode (500 + 30*i, 300 + (-1)^i*30*i)
            ) 
            [0..5]
        )
        [ (0, 1)
        , (0, 2)
        , (2, 3)
        , (3, 4)
        , (2, 5)
        , (2, 4)
        ]
    |> Graph.mapEdges (always {})
    |> Model

empty: Model
empty =
    Model Graph.empty

getNodePos: Graph.NodeId -> Model -> Maybe (Int, Int)
getNodePos id {graph} =
    case Graph.get id graph of
        Just {node, incoming, outgoing} ->
            Just node.label.pos
        Nothing -> Nothing


-- UPDATE

type Action
    = AddNode Node.Model
    | AddEdge Graph.NodeId Graph.NodeId Edge
    | StepLayout
    | TickNodes Time.Time
    | NodeAction Graph.NodeId Node.Action

update: Action -> Model -> Model
update action model =
    case action of
        AddNode node ->
            {model | graph = addUnconnectedNode node model.graph}
        AddEdge a b edge ->
            {model | graph = addEdge a b edge model.graph}
        StepLayout ->
            {model | graph = Layout.stepLayout model.graph}
        TickNodes dt ->
            {model | graph = Graph.mapNodes (Node.Tick dt |> Node.update) model.graph}
        NodeAction id nodeAction ->
            let
                updateCtx maybeCtx =
                    case maybeCtx of
                        Just ctx ->
                            Focus.update
                                (Graph.node => Graph.label)
                                (Node.update nodeAction)
                                ctx
                            |> Just
                        Nothing ->
                            Nothing
            in
                {model | graph = Graph.update id updateCtx model.graph}

addEdge: Graph.NodeId -> Graph.NodeId -> Edge -> Graph -> Graph
addEdge a b edge graph =
    let
        contextUpdate id maybectx =
            case maybectx of
                Nothing -> Nothing
                Just ctx -> Just
                    {ctx | incoming = IntDict.insert id edge ctx.incoming}
    in
        if a /= b then
            Graph.update a (contextUpdate b) graph
            |> Graph.update b (contextUpdate a)
        else
            graph

addUnconnectedNode: Node.Model -> Graph -> Graph
addUnconnectedNode node graph =
    let
        id =
            case Graph.nodeIdRange graph of
                Just (a, b) -> b + 1
                Nothing -> 1

        newNode = 
            {node = Graph.Node id node, incoming = IntDict.empty, outgoing = IntDict.empty}
    in
        Graph.insert newNode graph


-- VIEW

view: Signal.Address (Graph.NodeId, NodeBase.MouseAction) 
    -> Signal.Address Action -> Model -> Svg.Svg
view mouseAddress address {graph} =
    let
        toPositions list =
            List.filterMap (\id -> Graph.get id graph) list
            |> List.map (\ctx -> ctx.node.label.pos)

        edges =
            Graph.edges graph
            |> List.map (\{from, to, label} -> (max from to, min from to))
            |> List.Extra.dropDuplicates
            |> List.unzip
            |> (\(l1, l2) -> List.map2 (,) (toPositions l1) (toPositions l2))
            |> List.map (\(a, b) -> edgeForm a b)

        context id =
            NodeAction id
            |> Signal.forwardTo address
            |> Node.Context (Signal.forwardTo mouseAddress (\action -> (id, action)))

        nodes =
            Graph.nodes graph
            |> List.map (\{id, label} -> Node.view (context id) label)
    in
        Svg.g [] (edges ++ nodes)

edgeForm: (Int, Int) -> (Int, Int) -> Svg.Svg
edgeForm posA posB =
    SvgHelp.line posA posB 5 "#244F9F"
