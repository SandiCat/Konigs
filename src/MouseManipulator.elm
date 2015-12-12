module MouseManipulator
    (Model, testModel, Action (..), update, view)
    where

import GraphMap
import Time
import Node
import Graph
import Debug
import Html.Attributes
import Html
import Html.Events as Events
import Svg
import Svg.Attributes as Att
import FpsClock
import Signal
import NodeBase


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , fpsClock: FpsClock.Model
    , pos: (Int, Int)
    }

type State
    = NoOp
    | Connecting Graph.NodeId (Int, Int)

testModel: Model
testModel =
    Model GraphMap.testModel NoOp FpsClock.init (0, 0)


-- UPDATE

type Action
    = Move (Int, Int)
    | Tick Time.Time
    | GraphMapAction GraphMap.Action
    | NodeMouseAction (Graph.NodeId, NodeBase.MouseAction)
    | DoubleClick
    | Release

update: Action -> Model -> Model
update action model =
    case action of
        GraphMapAction graphMapAction ->
            { model | graphMap = GraphMap.update graphMapAction model.graphMap }
        Move pos ->
            case model.state of
                Connecting id pos' ->
                    { model 
                        | state = Connecting id pos
                        , pos = pos
                    }
                NoOp -> { model | pos = pos }
        DoubleClick ->
            { model | graphMap = GraphMap.update 
                (Node.testNode model.pos |> GraphMap.AddNode)
                model.graphMap
            }
        Release ->
            { model | state = NoOp}
        Tick dt ->
            { model
                | graphMap =
                    GraphMap.update GraphMap.StepLayout model.graphMap
                    |> GraphMap.update (GraphMap.TickNodes dt)
                , fpsClock = FpsClock.update dt
            }
        NodeMouseAction (id, mouseAction) ->
            case mouseAction of
                NodeBase.Down ->
                    let
                        pos = 
                            case GraphMap.getNodePos id model.graphMap of
                                Just pos -> pos
                                Nothing -> Debug.log "mouse action no id" (0, 0)
                    in
                        { model | state = Connecting id (fst pos, snd pos) }
                NodeBase.Up ->
                    case model.state of
                        NoOp -> model
                        Connecting id' pos ->
                            { model | 
                                graphMap = GraphMap.update 
                                    (GraphMap.AddEdge id id' {})
                                    model.graphMap
                                , state = NoOp
                            }


-- VIEW

view: Signal.Address Action -> (Int, Int) -> Model -> Html.Html
view address (w, h) model =
    let
        connection =
            case model.state of
                Connecting id pos ->
                    case Graph.get id model.graphMap.graph of
                        Nothing -> []
                        Just {incoming, node, outgoing} ->
                            [ GraphMap.edgeForm node.label.pos pos ]
                otherwise -> []

        svg =
            Svg.svg
                [ toString w |> Att.width
                , toString h |> Att.height
                ]
                (
                    connection
                    ++
                    [ GraphMap.view 
                        (Signal.forwardTo address NodeMouseAction)
                        (Signal.forwardTo address GraphMapAction)
                        model.graphMap
                    , FpsClock.view model.fpsClock
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
