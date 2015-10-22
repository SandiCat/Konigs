module Layout (stepLayout) where

import Graph
import IntDict
import Math.Vector2 as Vec
import Focus exposing ((=>))
import Time
import Node
import Debug

c1: Float
c1 = 50
c2: Float
c2 = 200
c3: Float
c3 = 500000
c4: Float
c4 = 1

nil: Vec.Vec2
nil =
    Vec.vec2 0 0

posToVec: (Int, Int) -> Vec.Vec2
posToVec (x, y) =
    Vec.vec2 (toFloat x) (toFloat y)

vecToPos: Vec.Vec2 -> (Int, Int)
vecToPos vec =
    (Vec.getX vec |> round, Vec.getY vec |> round)

type alias Context e =
    Graph.NodeContext Node.Model e

type alias Graph' e =
    Graph.Graph Node.Model e

nodeAttract: Context e -> Graph' e -> Vec.Vec2
nodeAttract ctx graph =
    let
        thisVec =
            posToVec ctx.node.label.pos

        calculateForce vec =
            Vec.direction thisVec vec
            |> Vec.scale (-c1 * (logBase 10 ((Vec.distance thisVec vec) / c2)))
    in
        IntDict.union ctx.incoming ctx.outgoing
        |> IntDict.keys
        |> List.filterMap (\id -> Graph.get id graph)
        |> List.map ((\ctx -> ctx.node.label.pos) >> posToVec >> calculateForce)
        |> List.foldr Vec.add nil

nodeRepulse: Context e -> Graph' e -> Vec.Vec2
nodeRepulse ctx graph =
    let
        neighbours =
            IntDict.union ctx.incoming ctx.outgoing

        thisVec =
            posToVec ctx.node.label.pos

        calculateForce vec =
            Vec.direction thisVec vec
            |> Vec.scale (c3 / (Vec.distance thisVec vec)^2)

        keep {id, label} =
            case Graph.get id graph of
                Nothing -> False
                Just {node, incoming, outgoing} ->
                    (id /= ctx.node.id)
                    && (IntDict.member id neighbours |> not)
                    && ((IntDict.isEmpty incoming |> not) || (IntDict.isEmpty outgoing |> not))
    in
        Graph.nodes graph
        |> List.filter keep
        |> List.map ((\node -> node.label.pos) >> posToVec >> calculateForce)
        |> List.foldr Vec.add nil

stepLayout: Graph' e -> Float -> Graph' e
stepLayout graph dt =
    let
        stepPos ctx pos =
            Vec.add (nodeRepulse ctx graph) (nodeAttract ctx graph)
            |> Vec.scale c4
            |> Vec.add (posToVec pos)
            |> vecToPos

        node =
            Focus.create .node (\f rec  -> {rec | node <- f rec.node})
        label =
            Focus.create .label (\f rec  -> {rec | label <- f rec.label})
        pos =
            Focus.create .pos (\f rec  -> {rec | pos <- f rec.pos})

        update ctx =
            if IntDict.isEmpty ctx.incoming && IntDict.isEmpty ctx.outgoing then
                ctx
            else
                Focus.update (node => label => pos)  (stepPos ctx) ctx
    in
        Graph.mapContexts update graph