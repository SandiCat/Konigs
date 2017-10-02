module Layout exposing (stepLayout, drawForces)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import Color
import Graph
import IntDict
import Focus exposing ((=>))
import Node
import Util.Misc
import Math.Vector2 as Vec2 exposing (Vec2)


c1 : Float
c1 =
    100


c2 : Float
c2 =
    200


c3 : Float
c3 =
    500000


c4 : Float
c4 =
    1


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
                |> Vec2.scale (-c1 * (logBase 10 ((Vec2.distance thisVec vec) / c2)))
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
                |> Vec2.scale (c3 / (Vec2.distance thisVec vec) ^ 2)

        keep { id, label } =
            case Graph.get id graph of
                Nothing ->
                    False

                Just { node, incoming, outgoing } ->
                    (id /= ctx.node.id)
                        && (IntDict.member id neighbours |> not)
                        && ((IntDict.isEmpty incoming |> not) || (IntDict.isEmpty outgoing |> not))
    in
        if IntDict.isEmpty ctx.incoming && IntDict.isEmpty ctx.outgoing then
            []
        else
            Graph.nodes graph
                |> List.filter keep
                |> List.map ((\node -> node.label.pos) >> calculateForce)


stepLayout : Graph_ e -> Graph_ e
stepLayout graph =
    let
        stepPos ctx pos =
            (nodeRepulse ctx graph ++ nodeAttract ctx graph)
                |> List.foldr Vec2.add nil
                |> Vec2.scale c4
                |> Vec2.add pos

        pos =
            Focus.create .pos (\f rec -> { rec | pos = f rec.pos })

        update ctx =
            Focus.update
                (Util.Misc.nodeFocus => Util.Misc.labelFocus => pos)
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
