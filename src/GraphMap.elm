module GraphMap (Model, testModel, Action (..), update, view, Graph, edgeForm) where

import Graph
import IntDict
import Node
import Debug
import Svg
import Svg.Attributes as Att
import List.Extra
import Layout
import Time


-- MODEL

type alias Model =
    { graph: Graph
    }

type alias Graph =
    Graph.Graph Node.Model Edge

type alias Edge = {}

testModel: Model
testModel =
    let
        nodes = List.map2 (\pos id -> Node.plainNode pos |> Graph.Node id)
            [(600 - 100, 400), (600, 400 - 100), (600, 400 + 100)]
            [1..3]
    in
        Graph.fromNodesAndEdges nodes [(Graph.Edge 1 2 (Edge))]
        |> Model

empty: Model
empty =
    Model Graph.empty


-- UPDATE

type Action
    = AddNode Node.Model
    | AddEdge Graph.NodeId Graph.NodeId Edge
    | StepLayout Time.Time

update: Action -> Model -> Model
update action model =
    case action of
        AddNode node ->
            {model | graph <- addUnconnectedNode node model.graph}
        AddEdge a b edge ->
            {model | graph <- addEdge a b edge model.graph}
        StepLayout dt ->
            {model | graph <- Layout.stepLayout model.graph dt}

addEdge: Graph.NodeId -> Graph.NodeId -> Edge -> Graph -> Graph
addEdge a b edge graph =
    let
        contextUpdate id maybectx =
            case maybectx of
                Nothing -> Nothing
                Just ctx -> Just
                    {ctx | incoming <- IntDict.insert id edge ctx.incoming}
    in
        Graph.update a (contextUpdate b) graph
        |> Graph.update b (contextUpdate a)

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

view: Model -> Svg.Svg
view model =
    showGraph model.graph

showGraph: Graph -> Svg.Svg
showGraph graph =
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

        nodes =
            Graph.nodes graph
            |> List.map .label
            |> List.map (Node.view {})
    in
        Svg.g [] (edges ++ nodes)


edgeForm: (Int, Int) -> (Int, Int) -> Svg.Svg
edgeForm (x, y) (x', y') =
    Svg.line
        [ toString x |> Att.x1
        , toString y |> Att.y1
        , toString x' |> Att.x2
        , toString y' |> Att.y2
        , Att.stroke "black"
        , Att.fill "transparent"
        , Att.strokeWidth "5"
        ]
        []
