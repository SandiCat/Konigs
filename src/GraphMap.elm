module GraphMap exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Events as SvgEvents
import TypedSvg.Types as SvgTypes
import TypedSvg.Core as SvgCore exposing (Svg)
import Graph exposing (Graph)
import IntDict
import Node
import Color
import Layout
import AnimationFrame
import Time
import Focus exposing ((=>))
import Util.Cmd
import Html
import Html.Events as Events
import Util.Css
import MyCss
import Css
import Util
import Math.Vector2 as Vec2 exposing (Vec2)
import Edge
import Util.Graph exposing (EdgeId)
import Mouse
import OutMessage
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode


-- MODEL


type alias Model =
    { graph : Graph Node.Model Edge.Model
    , state : State
    , mousePos : Vec2
    , cameraPos : Vec2
    }


type State
    = None
    | Connecting Graph.NodeId
    | MovingCamera Vec2 Vec2


fullInit :
    Vec2
    -> List ( Graph.Node Node.Model, Cmd Node.Msg )
    -> List ( Graph.Edge Edge.Model, Cmd Edge.Msg )
    -> ( Model, Cmd Msg )
fullInit camera nodeStates edgeStates =
    let
        ( nodes, nodeCmds ) =
            List.map
                (\( node, cmd ) ->
                    ( node, Cmd.map (NodeMsg node.id) cmd )
                )
                nodeStates
                |> List.unzip

        ( edges, edgeCmds ) =
            List.map
                (\( edge, cmd ) ->
                    ( edge, Util.Graph.EdgeId edge.from edge.to |> EdgeMsg |> (flip Cmd.map) cmd )
                )
                edgeStates
                |> List.unzip
    in
        { graph =
            Graph.fromNodesAndEdges nodes edges
                |> Layout.randomlyArrange
        , state = None
        , mousePos = Vec2.vec2 0 0
        , cameraPos = camera
        }
            ! (edgeCmds ++ nodeCmds)


init : ( Model, Cmd Msg )
init =
    fullInit
        (Vec2.vec2 0 0)
        (List.map
            (\i ->
                Node.init i ("Test node " ++ toString i) (Vec2.vec2 0 0)
                    |> Tuple.mapFirst (Graph.Node i)
            )
            (List.range 0 5)
        )
        ([ ( 0, 1 ), ( 0, 2 ), ( 2, 3 ), ( 3, 4 ), ( 2, 5 ), ( 2, 4 ) ]
            |> List.map (\( a, b ) -> Tuple.mapFirst (Graph.Edge a b) Edge.init)
         -- direction is arbitrary, see docs
        )


getNodePos : Graph.NodeId -> Graph Node.Model e -> Vec2
getNodePos id graph =
    case Graph.get id graph of
        Just { node, incoming, outgoing } ->
            node.label.pos

        Nothing ->
            Vec2.vec2 0 0
                |> Debug.crash "getNodePos got nonexisting id!"


offsetMouse : Model -> Vec2
offsetMouse model =
    Vec2.sub model.mousePos model.cameraPos



-- JSON


extractCmd : { record | label : ( l, cmd ) } -> ( { record | label : l }, cmd )
extractCmd record =
    ( { record | label = Tuple.first record.label }, Tuple.second record.label )


decode : Decode.Decoder ( Model, Cmd Msg )
decode =
    Decode.succeed fullInit
        |: Decode.field "camera" Util.decodeVec2
        |: Decode.field "nodes"
            (Json.Decode.Extra.indexedList
                (\i ->
                    Node.decode i
                        |> Util.Graph.decodeNode
                        |> Decode.map extractCmd
                )
            )
        |: Decode.field "edges"
            (Util.Graph.decodeEdge Edge.decode
                |> Decode.map extractCmd
                |> Decode.list
            )


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "camera", Util.encodeVec2 model.cameraPos )
        , ( "nodes"
          , Graph.nodes model.graph
                |> List.map (Util.Graph.encodeNode Node.encode)
                |> Encode.list
          )
        , ( "edges"
          , Graph.edges model.graph
                |> List.map (Util.Graph.encodeEdge Edge.encode)
                |> Encode.list
          )
        ]



-- UPDATE


type Msg
    = NoOp
    | StepLayout Time.Time
    | NodeMsg Graph.NodeId Node.Msg
    | EdgeMsg Util.Graph.EdgeId Edge.Msg
    | Doubleclick
    | Hold
    | Release
    | Move Vec2
    | LeaveWindow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        StepLayout dt ->
            { model | graph = Layout.stepLayout model.graph } ! []

        NodeMsg id nodeMsg ->
            let
                focusUpdate node ctx =
                    Focus.update
                        (Util.nodeFocus => Util.labelFocus)
                        (always node)
                        ctx

                setNode newNode =
                    { model
                        | graph = Graph.update id (focusUpdate newNode |> Maybe.map) model.graph
                    }

                update_ ctx =
                    Node.update nodeMsg ctx.node.label
                        |> OutMessage.mapComponent setNode
                        |> OutMessage.mapCmd (NodeMsg id)
                        |> OutMessage.evaluateMaybe (updateNodeOutMsg id) Cmd.none
            in
                Graph.get id model.graph
                    |> Maybe.map update_
                    |> Maybe.withDefault (model ! [])

        EdgeMsg id edgeMsg ->
            let
                setEdge newEdge =
                    { model
                        | graph = Util.Graph.updateEdge id (always newEdge) model.graph
                    }

                update_ edge =
                    Edge.update edgeMsg edge
                        |> OutMessage.mapComponent setEdge
                        |> OutMessage.mapCmd (EdgeMsg id)
                        |> OutMessage.evaluateMaybe (updateEdgeOutMsg id) Cmd.none
            in
                Util.Graph.getEdge id model.graph
                    |> Maybe.map update_
                    |> Maybe.withDefault (model ! [])

        Doubleclick ->
            let
                id =
                    Util.Graph.newNodeId model.graph

                ( node, nodeCmd ) =
                    Node.init id "" <| offsetMouse model
            in
                { model | graph = Util.Graph.addUnconnectedNode id node model.graph }
                    ! [ nodeCmd |> Cmd.map (NodeMsg id) ]

        Hold ->
            case model.state of
                None ->
                    { model | state = MovingCamera model.mousePos model.cameraPos } ! []

                _ ->
                    model ! []

        Release ->
            { model | state = None } ! []

        Move newMousePos ->
            case model.state of
                MovingCamera mousePos cameraPos ->
                    { model | cameraPos = Vec2.add cameraPos (Vec2.sub newMousePos mousePos) } ! []

                _ ->
                    { model | mousePos = newMousePos } ! []

        LeaveWindow ->
            { model | state = None } ! []


updateNodeOutMsg : Graph.NodeId -> Node.OutMsg -> Model -> ( Model, Cmd Msg )
updateNodeOutMsg id msg model =
    case msg of
        Node.MouseDown ->
            { model | state = Connecting id } ! []

        Node.MouseUp ->
            case model.state of
                Connecting id_ ->
                    let
                        ( edge, edgeCmd ) =
                            Edge.init

                        edgeId =
                            -- order is arbitrary, see docs
                            Util.Graph.EdgeId id id_
                    in
                        { model
                            | graph = Util.Graph.addEdge edgeId edge model.graph
                            , state = None
                        }
                            ! [ edgeCmd |> Cmd.map (EdgeMsg edgeId) ]

                _ ->
                    model ! []

        Node.Remove ->
            { model | graph = Graph.remove id model.graph } ! []


updateEdgeOutMsg : Util.Graph.EdgeId -> Edge.OutMsg -> Model -> ( Model, Cmd Msg )
updateEdgeOutMsg id outMsg model =
    case outMsg of
        Edge.Remove ->
            { model | graph = Util.Graph.removeEdge id model.graph } ! []



-- VIEW


view : Util.Size -> Model -> Html.Html Msg
view size model =
    let
        singleEdge view { from, to, label } =
            view (getNodePos from model.graph) (getNodePos to model.graph) label
                |> Html.map (EdgeMsg <| Util.Graph.EdgeId from to)

        edges view =
            Graph.edges model.graph
                |> List.map (singleEdge view)

        connectEdge =
            case model.state of
                Connecting id ->
                    [ Tuple.first Edge.init
                        |> Edge.svgView (offsetMouse model) (getNodePos id model.graph)
                        -- the connecting edge doesn't need to handle messages
                        |> Html.map (always NoOp)
                    ]

                _ ->
                    []

        nodes view =
            Graph.nodes model.graph
                |> List.map (\{ id, label } -> view label |> Html.map (NodeMsg id))
    in
        Util.Css.layers 0
            [ Util.Css.userSelect False
            , Events.onMouseLeave LeaveWindow
            ]
            [ Svg.svg
                [ onDoubleClick Doubleclick
                , Util.Css.userSelect True
                , SvgAttPx.width <| toFloat size.width
                , SvgAttPx.height <| toFloat size.height
                , SvgEvents.onMouseUp Release
                , SvgEvents.onMouseDown Hold
                ]
                [ Svg.g
                    [ SvgAtt.transform
                        [ SvgTypes.Translate
                            (Vec2.getX model.cameraPos)
                            (Vec2.getY model.cameraPos)
                        ]
                    ]
                    (edges Edge.svgView ++ connectEdge ++ nodes Node.svgView)
                ]
            , Html.div
                [ MyCss.class [ MyCss.GraphMap ]
                , Util.Css.style
                    [ Vec2.getX model.cameraPos |> Css.px |> Css.left
                    , Vec2.getY model.cameraPos |> Css.px |> Css.top
                    ]
                ]
                (edges Edge.view ++ nodes Node.view)
            ]


onDoubleClick : Msg -> SvgCore.Attribute Msg
onDoubleClick msg =
    SvgEvents.on "dblclick" <| Decode.succeed msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Mouse.moves (\{ x, y } -> Move <| Vec2.vec2 (toFloat x) (toFloat y))
            :: Mouse.ups (always Release)
            {- This stops the connecting edge from randomly hanging in the air,
               but it's extremly questionable. What if this Msg comes before
               Node.MouseUp? It doesn't ever happen, but it's hacky.
            -}
            :: AnimationFrame.diffs StepLayout
            :: List.map
                (\{ id, label } -> Sub.map (NodeMsg id) (Node.subscriptions label))
                (Graph.nodes model.graph)
        )
