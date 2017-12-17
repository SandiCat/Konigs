module Util.Graph exposing (..)

import Graph exposing (Graph)
import IntDict exposing (IntDict)
import Set exposing (Set)


type alias EdgeId =
    { from : Graph.NodeId, to : Graph.NodeId }


addEdge : EdgeId -> e -> Graph n e -> Graph n e
addEdge { from, to } edge graph =
    let
        exists =
            case Graph.get from graph of
                Just ctx ->
                    IntDict.member to ctx.outgoing

                Nothing ->
                    False

        contextUpdate id ctx =
            { ctx | outgoing = IntDict.insert id edge ctx.outgoing }
    in
        if from /= to && not exists then
            Graph.update from (contextUpdate to |> Maybe.map) graph
        else
            graph


newNodeId : Graph n e -> Graph.NodeId
newNodeId graph =
    case Graph.nodeIdRange graph of
        Just ( a, b ) ->
            b + 1

        Nothing ->
            1


addUnconnectedNode : Graph.NodeId -> n -> Graph n e -> Graph n e
addUnconnectedNode id node graph =
    Graph.insert
        { node = Graph.Node id node
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }
        graph


getEdge : EdgeId -> Graph n e -> Maybe e
getEdge { from, to } graph =
    Graph.get from graph
        |> Maybe.andThen (\ctx -> IntDict.get to ctx.outgoing)


removeEdge : EdgeId -> Graph n e -> Graph n e
removeEdge { from, to } graph =
    let
        updateCtx ctx =
            { ctx | outgoing = IntDict.remove to ctx.outgoing }
    in
        Graph.update from (Maybe.map updateCtx) graph


existsEdge : EdgeId -> Graph n e -> Bool
existsEdge { from, to } graph =
    Graph.get from graph
        |> Maybe.andThen (.outgoing >> IntDict.member to >> Just)
        |> Maybe.withDefault False


updateEdge : EdgeId -> (e -> e) -> Graph n e -> Graph n e
updateEdge id update graph =
    let
        updateCtx ctx =
            { ctx | outgoing = IntDict.update id.to (Maybe.map update) ctx.outgoing }
    in
        Graph.update id.from (Maybe.map updateCtx) graph


{-| Returns the IDs of all nodes connected to the given node, treating every edge
as bidirectional. BFS or close to it, should be linear or O(n log n) in the number
of nodes. Didn't find a good way to get this with the Graph package so I wrote my own.
-}
connectedNodes : Graph.NodeId -> Graph n e -> List Graph.NodeId
connectedNodes id graph =
    connectedNodesRec graph Set.empty [ id ]
        |> Set.toList


connectedNodesRec : Graph n e -> Set Graph.NodeId -> List Graph.NodeId -> Set Graph.NodeId
connectedNodesRec graph visited queue =
    case queue of
        id :: rest ->
            Graph.get id graph
                |> Maybe.map (\ctx -> IntDict.union ctx.incoming ctx.outgoing)
                |> Maybe.withDefault IntDict.empty
                |> IntDict.keys
                |> List.filter (\id -> Set.member id visited |> not)
                |> (++) rest
                |> connectedNodesRec graph (Set.insert id visited)

        [] ->
            visited
