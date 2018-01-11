module Layout exposing (stepLayout, drawForces, randomlyArange)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import Color
import Graph
import IntDict
import Focus exposing ((=>))
import Node
import Util
import Util.Graph
import Math.Vector2 as Vec2 exposing (Vec2)
import Random


const =
    { c1 = 100
    , c2 = 200
    , c3 = 500000
    , c4 = 1
    }


nil : Vec2
nil =
    Vec2.vec2 0 0


type alias Context e =
    Graph.NodeContext Node.Model e


type alias Graph_ e =
    Graph.Graph Node.Model e


nodeAttract : Context e -> Graph_ e -> List Vec2
nodeAttract ctx graph =
    let
        thisVec =
            ctx.node.label.pos

        calculateForce vec =
            Vec2.direction thisVec vec
                |> Vec2.scale (-const.c1 * (logBase 10 ((Vec2.distance thisVec vec) / const.c2)))
    in
        IntDict.union ctx.incoming ctx.outgoing
            |> IntDict.keys
            |> List.filterMap (\id -> Graph.get id graph)
            |> List.map ((\ctx -> ctx.node.label.pos) >> calculateForce)


nodeRepulse : Context e -> Graph_ e -> List Vec2
nodeRepulse ctx graph =
    let
        neighbours =
            IntDict.union ctx.incoming ctx.outgoing

        thisVec =
            ctx.node.label.pos

        calculateForce vec =
            Vec2.direction thisVec vec
                |> Vec2.scale (const.c3 / (Vec2.distance thisVec vec) ^ 2)

        keep { node, incoming, outgoing } =
            (node.id /= ctx.node.id) && (IntDict.member node.id neighbours |> not)
    in
        Util.Graph.connectedNodes ctx.node.id graph
            |> List.filterMap (\id -> Graph.get id graph)
            |> List.filter keep
            |> List.map (.node >> .label >> .pos >> calculateForce)


posFocus : Focus.Focus { b | pos : a } a
posFocus =
    Focus.create .pos (\f rec -> { rec | pos = f rec.pos })


stepLayout : Graph_ e -> Graph_ e
stepLayout graph =
    let
        stepPos ctx pos =
            (nodeRepulse ctx graph ++ nodeAttract ctx graph)
                |> List.foldr Vec2.add nil
                |> Vec2.scale const.c4
                |> Vec2.add pos

        update ctx =
            Focus.update
                (Util.nodeFocus => Util.labelFocus => posFocus)
                (stepPos ctx)
                ctx
    in
        Graph.mapContexts update graph


drawForces : Graph_ e -> Svg msg
drawForces graph =
    let
        singleForce pos vec color =
            Svg.line
                [ Vec2.getX pos |> SvgAttPx.x1
                , Vec2.getY pos |> SvgAttPx.y1
                , Vec2.getX vec + Vec2.getX pos |> SvgAttPx.x2
                , Vec2.getY vec + Vec2.getY pos |> SvgAttPx.y2
                , SvgAtt.stroke color
                , SvgAttPx.strokeWidth 3
                ]
                []

        singleNode ctx color forceF =
            forceF ctx graph
                |> List.map
                    (\vec -> singleForce ctx.node.label.pos (Vec2.scale 30 vec) color)
    in
        Graph.fold
            (\ctx acc ->
                singleNode ctx Color.green nodeAttract
                    :: singleNode ctx Color.red nodeRepulse
                    :: acc
            )
            []
            graph
            |> List.concat
            |> Svg.g []


randomlyArrange : Graph_ e -> Graph_ e
randomlyArrange graph =
    let
        nodes =
            Graph.nodes graph

        coordinateGen =
            -- the range could be tailored to width and height, but it should converge either way
            Random.float 0 600

        randomPositions =
            Random.pair coordinateGen coordinateGen
                |> Random.map Vec2.fromTuple
                |> Random.list (List.length nodes)
                -- the seed could be kept in state and updated if variation is needed, but it's not
                |> (flip Random.step) (Random.initialSeed 1337)
                |> Tuple.first

        update node pos =
            Focus.update
                (Util.labelFocus => posFocus)
                (always pos)
                node
    in
        -- there's no better way than taking the graph apart and putting it back together
        -- Graph.map2Nodes is not provided
        Graph.fromNodesAndEdges
            (List.map2 update nodes randomPositions)
            (Graph.edges graph)
