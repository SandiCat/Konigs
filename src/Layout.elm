module Layout (stepLayout, drawForces) where

import Graph
import IntDict
import Math.Vector2 as Vec
import Focus exposing ((=>))
import Node
import Svg
import Svg.Attributes as Att

c1: Float
c1 = 100
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

nodeAttract: Context e -> Graph' e -> List Vec.Vec2
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

nodeRepulse: Context e -> Graph' e -> List Vec.Vec2
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
        if IntDict.isEmpty ctx.incoming && IntDict.isEmpty ctx.outgoing then
            []
        else
            Graph.nodes graph
            |> List.filter keep
            |> List.map ((\node -> node.label.pos) >> posToVec >> calculateForce)

stepLayout: Graph' e -> Graph' e
stepLayout graph =
    let
        stepPos ctx pos =
            (nodeRepulse ctx graph ++ nodeAttract ctx graph)
            |> List.foldr Vec.add nil
            |> Vec.scale c4
            |> Vec.add (posToVec pos)
            |> vecToPos

        node =
            Focus.create .node (\f rec -> {rec | node = f rec.node})
        label =
            Focus.create .label (\f rec -> {rec | label = f rec.label})
        pos =
            Focus.create .pos (\f rec -> {rec | pos = f rec.pos})

        update ctx =
            Focus.update (node => label => pos) (stepPos ctx) ctx
    in
        Graph.mapContexts update graph

drawForces: Graph' e -> Svg.Svg
drawForces graph =
    let
        singleForce (x, y) vec color =
            Svg.line
                [ toString x |> Att.x1
                , toString y |> Att.y1
                , Vec.getX vec |> round |> (+) x |> toString |> Att.x2
                , Vec.getY vec |> round |> (+) y |> toString |> Att.y2
                , Att.stroke color
                , Att.strokeWidth "3"
                ]
                []

        singleNode ctx color forceF =
            forceF ctx graph
            |> List.map 
                (\vec -> singleForce ctx.node.label.pos (Vec.scale 30 vec) color)
    in
        Graph.fold
            ( \ctx acc ->
                singleNode ctx "yellow" nodeAttract
                :: singleNode ctx "red" nodeRepulse
                :: acc )
            []
            graph
        |> List.concat
        |> Svg.g []