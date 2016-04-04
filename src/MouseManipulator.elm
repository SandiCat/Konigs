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
import Css exposing (px)
import CssStuff.Util as CssUtil exposing (ipx)


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , mousePos: (Int, Int)
    , size: {w: Int, h: Int}
    , origin: {xo: Int, yo: Int} -- camera position
    }

type State
    = NoOp
    | Connecting Graph.NodeId
    | MovingCamera (Int, Int) {xo: Int, yo: Int}

offsetMouse: Model -> (Int, Int)
offsetMouse model =
    ( fst model.mousePos - model.origin.xo
    , snd model.mousePos - model.origin.yo
    )

init: (Model, Effects Action)
init =
    EffectsUtil.update
        (\x -> Model x NoOp (0, 0) {w = 0, h = 0} {xo = 0, yo = 0})
        GraphMapAction
        GraphMap.init


-- UPDATE

type Action
    = Move (Int, Int)
    | GraphMapAction GraphMap.Action
    | NodeMouseAction (Graph.NodeId, NodeBase.MouseAction)
    | DoubleClick
    | Hold
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
        Move (x, y) ->
            case model.state of
                MovingCamera (xm, ym) {xo, yo} ->
                    EffectsUtil.noFx { model | origin = {xo = xo + x - xm, yo = yo + y - ym} }
                _ -> EffectsUtil.noFx { model | mousePos = (x, y) }
        DoubleClick -> 
            GraphMap.update 
                (offsetMouse model |> Node.testNode |> GraphMap.AddNode)
                model.graphMap
            |> EffectsUtil.update
                (\x -> { model | graphMap = x })
                GraphMapAction
        Hold ->
            case model.state of
                NoOp ->
                    EffectsUtil.noFx { model | state = MovingCamera model.mousePos model.origin }
                _ ->
                    EffectsUtil.noFx model
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
                        EffectsUtil.noFx { model | state = Connecting id }
                NodeBase.Up ->
                    case model.state of
                        Connecting id' ->
                            GraphMap.update 
                                (GraphMap.AddEdge id id' {})
                                model.graphMap
                            |> EffectsUtil.update
                                (\x -> { model | graphMap = x })
                                GraphMapAction
                        _ -> EffectsUtil.noFx model


-- VIEW

view: Signal.Address Action -> Model -> Html.Html
view address model =
    let
        connection =
            case model.state of
                Connecting id ->
                    case GraphMap.getNodePos id model.graphMap.graph of
                        Nothing -> []
                        Just nodePos ->
                            [ GraphMap.edgeForm nodePos (offsetMouse model) ]
                _ -> []

        svg =
            Svg.svg
                [ toString model.size.w |> Att.width
                , toString model.size.h |> Att.height
                , CssUtil.style
                    [ ipx model.origin.xo |> Css.left
                    , ipx model.origin.yo |> Css.top
                    , Css.position Css.absolute
                    ]
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
            , Events.onMouseDown address Hold
            , Events.onDoubleClick address DoubleClick
            , CssUtil.style
                [ Css.width (ipx model.size.w)
                , Css.height (ipx model.size.h)
                ]
            ]
            [ toString model.origin.xo ++ ", " ++ toString model.origin.yo |> Html.text 
            , svg
            ]

unselectableStyle: Html.Attribute
unselectableStyle =
    Html.Attributes.style
        [ ("-moz-user-select", "none")
        , ("-webkit-user-select", "none")
        , ("-ms-user-select", "none")
        ]
