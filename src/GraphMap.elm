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
import Effects exposing (Effects)


-- MODEL

type alias Model =
    { graph: Graph
    }

type alias Graph =
    Graph.Graph Node.Model Edge

type alias Edge = {}

init: (Model, Effects Action)
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

        model =         
            Graph.fromNodeLabelsAndEdgePairs
                nodes
                [ (0, 1)
                , (0, 2)
                , (2, 3)
                , (3, 4)
                , (2, 5)
                , (2, 4)
                ]
            |> Graph.mapEdges (always {})
            |> Model

        fxs =
            List.map2 (\i fx -> Effects.map (NodeAction i) fx) range nodeFxs
            |> (::) (Effects.tick StepLayout)
            |> Effects.batch
    in
        (model, fxs)

empty: (Model, Effects Action)
empty =
    (Model Graph.empty, Effects.none)

getNodePos: Graph.NodeId -> Model -> Maybe (Int, Int)
getNodePos id {graph} =
    case Graph.get id graph of
        Just {node, incoming, outgoing} ->
            Just node.label.pos
        Nothing -> Nothing


-- UPDATE

type Action
    = AddNode (Node.Model, Effects Node.Action)
    | AddEdge Graph.NodeId Graph.NodeId Edge
    | StepLayout Time.Time
    | NodeAction Graph.NodeId Node.Action

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        AddNode (node, fx) ->
            let
                (graph, id) =
                    addUnconnectedNode node model.graph
            in   
                ( { model | graph = graph }
                , Effects.map (NodeAction id) fx
                )
        AddEdge a b edge ->
            ({model | graph = addEdge a b edge model.graph}, Effects.none)
        StepLayout dt ->
            ({model | graph = Layout.stepLayout model.graph}
            , Effects.tick StepLayout
            )
        NodeAction id nodeAction ->
            {-
                Well, ain't this fugly. Graph keep Node.Model, but Node.update returns
                (Model, Effects Action). This is all standard Elm Architecture stuff.
                A slight deviation would be having the Graph keep the tuple, but this would
                complicate all other graph modifications that involve node manipulations,
                i.e. all of them. So, Debug.crash and ugliness it is. I think monads are ment
                for doing this type of stuff. It could be a hole in the language.
            -}
            let
                (updatedNode, fx) =
                    case Graph.get id model.graph of
                        Nothing ->
                            Debug.crash "unknown id in NodeAction GraphMap update"
                        Just ctx ->
                            Node.update nodeAction ctx.node.label

                updateCtx maybeCtx =
                    case maybeCtx of
                        Just ctx ->
                            Focus.update
                                (Graph.node => Graph.label)
                                (always updatedNode)
                                ctx
                            |> Just
                        Nothing ->
                            Nothing
            in
                ( {model | graph = Graph.update id updateCtx model.graph}
                , Effects.map (NodeAction id) fx
                )

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
