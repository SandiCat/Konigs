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
import EffectsUtil


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , pos: (Int, Int) -- mouse position
    , size: {w: Int, h: Int}
    }

type State
    = NoOp
    | Connecting Graph.NodeId (Int, Int)

init: (Model, Effects Action)
init =
    EffectsUtil.update
        (\x -> Model x NoOp (0, 0) {w = 0, h = 0})
        GraphMapAction
        GraphMap.init


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
            EffectsUtil.update
                (\x -> { model | graphMap = x })
                GraphMapAction
                (GraphMap.update graphMapAction model.graphMap)
        Move pos ->
            case model.state of
                Connecting id pos' ->
                    { model 
                        | state = Connecting id pos
                        , pos = pos }
                    |> EffectsUtil.noFx
                NoOp -> EffectsUtil.noFx { model | pos = pos }
        DoubleClick -> 
            GraphMap.update 
                (Node.testNode model.pos |> GraphMap.AddNode)
                model.graphMap
            |> EffectsUtil.update
                (\x -> { model | graphMap = x })
                GraphMapAction
        Release ->
            EffectsUtil.noFx { model | state = NoOp }
        Resize (w, h) ->
            EffectsUtil.noFx { model | size = {w = w, h = h} }
        NodeMouseAction (id, mouseAction) ->
            case mouseAction of
                NodeBase.Down ->
                    let
                        pos = 
                            case GraphMap.getNodePos id model.graphMap.graph of
                                Just pos -> pos
                                Nothing -> Debug.log "mouse action no id" (0, 0)
                    in
                        EffectsUtil.noFx { model | state = Connecting id (fst pos, snd pos) }
                NodeBase.Up ->
                    case model.state of
                        NoOp -> EffectsUtil.noFx model
                        Connecting id' pos ->
                            GraphMap.update 
                                (GraphMap.AddEdge id id' {})
                                model.graphMap
                            |> EffectsUtil.update
                                (\x -> { model | graphMap = x })
                                GraphMapAction


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
                _ -> []

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
