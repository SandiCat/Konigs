module MouseManipulator
    (Model, init, Action (..), update, view)
    where

import GraphMap
import Node
import Graph
import Html.Attributes
import Html
import Html.Events as Events
import Svg
import Svg.Attributes as Att
import NodeBase
import Effects exposing (Effects)


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , pos: (Int, Int)
    , size: {w: Int, h: Int}
    }

type State
    = NoOp
    | Connecting Graph.NodeId (Int, Int)

init: (Model, Effects Action)
init =
    let
        (graphMap, graphFx) = GraphMap.init
    in
        ( Model graphMap NoOp (0, 0) {w = 0, h = 0}
            -- the dimensions change at startup to match window's, hence the 0 0
        , Effects.map GraphMapAction graphFx
        )


-- UPDATE

type Action
    = Move (Int, Int)
    | GraphMapAction GraphMap.Action
    | NodeMouseAction (Graph.NodeId, NodeBase.MouseAction)
    | DoubleClick
    | Release
    | Resize (Int, Int)

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        GraphMapAction graphMapAction ->
            let
                (graphMap, fx) = GraphMap.update graphMapAction model.graphMap
            in
                ({ model | graphMap = graphMap }, Effects.map GraphMapAction fx)
        Move pos ->
            case model.state of
                Connecting id pos' ->
                    ( { model 
                            | state = Connecting id pos
                            , pos = pos }
                    , Effects.none
                    )
                NoOp -> ({ model | pos = pos }, Effects.none)
        DoubleClick ->
            let
                (graphMap, fx) = 
                    GraphMap.update 
                        (Node.testNode model.pos |> GraphMap.AddNode)
                        model.graphMap
            in
                ({ model | graphMap = graphMap }, Effects.map GraphMapAction fx)
        Release ->
            ({ model | state = NoOp }, Effects.none)
        Resize (w, h) ->
            ({ model | size = {w = w, h = h} }, Effects.none)
        NodeMouseAction (id, mouseAction) ->
            case mouseAction of
                NodeBase.Down ->
                    let
                        pos = 
                            case GraphMap.getNodePos id model.graphMap of
                                Just pos -> pos
                                Nothing -> Debug.log "mouse action no id" (0, 0)
                    in
                        ( { model | state = Connecting id (fst pos, snd pos) }
                        , Effects.none
                        )
                NodeBase.Up ->
                    case model.state of
                        NoOp -> (model, Effects.none)
                        Connecting id' pos ->
                            let
                                (graphMap, fx) =
                                    GraphMap.update 
                                        (GraphMap.AddEdge id id' {})
                                        model.graphMap
                            in
                                ({ model | graphMap = graphMap }, Effects.map GraphMapAction fx)


-- VIEW

view: Signal.Address Action -> Model -> Html.Html
view address model =
    let
        connection =
            case model.state of
                Connecting id pos ->
                    case Graph.get id model.graphMap.graph of
                        Nothing -> []
                        Just {incoming, node, outgoing} ->
                            [ GraphMap.edgeForm node.label.pos pos ]
                otherwise -> []
                -- TODO: make the optionality of this with a Maybe, not this list fuckery

        svg =
            Svg.svg
                [ toString model.size.w |> Att.width
                , toString model.size.h |> Att.height
                ]
                (
                    connection
                    ++
                    [ GraphMap.view 
                        (Signal.forwardTo address NodeMouseAction)
                        (Signal.forwardTo address GraphMapAction)
                        model.graphMap
                    ]
                )
    in
        Html.div 
            [ unselectableStyle
            , Events.onMouseUp address Release
            , Events.onDoubleClick address DoubleClick
            ]
            [ svg ]

unselectableStyle: Html.Attribute
unselectableStyle =
    Html.Attributes.style
        [ ("-moz-user-select", "none")
        , ("-webkit-user-select", "none")
        , ("-ms-user-select", "none")
        ]
