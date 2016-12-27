module MouseManipulator exposing
    ( init, update, view, subscriptions )

import GraphMap
import Node
import Graph
import Html.Attributes
import Html
import Html.Events as Events
import CmdUtil
import Css exposing (px)
import CssUtil exposing (ipx)
import Html.App
import Mouse
import Window
import Task


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , mousePos: (Int, Int)
    , size: {width: Int, height: Int}
    , cameraPos: {xo: Int, yo: Int} -- camera position
    }

type State
    = NoOp
    | Connecting Graph.NodeId
    | MovingCamera (Int, Int) {xo: Int, yo: Int}

offsetMouse: Model -> (Int, Int)
offsetMouse model =
    ( fst model.mousePos - model.cameraPos.xo
    , snd model.mousePos - model.cameraPos.yo
    )

init: (Model, Cmd Msg)
init =
    let
        (graphMap, gmCmd) = GraphMap.init
    in
        Model graphMap NoOp (0, 0) {width = 0, height = 0} {xo = 0, yo = 0} !
            [ gmCmd |> Cmd.map GraphMapMsg
            , Task.perform (Resize {width = 1000, height = 500} |> always) Resize Window.size
            ]


-- UPDATE

type Msg
    = Move (Int, Int)
    | GraphMapMsg GraphMap.Msg
    | DoubleClick
    | Hold
    | Release
    | Resize {width: Int, height: Int}

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GraphMapMsg graphMapMsg ->
            GraphMap.update graphMapMsg model.graphMap
            |> updateGraphMapHelp model
        Move (x, y) ->
            case model.state of
                MovingCamera (xm, ym) {xo, yo} ->
                    CmdUtil.noCmd { model | cameraPos = {xo = xo + x - xm, yo = yo + y - ym} }
                _ -> CmdUtil.noCmd { model | mousePos = (x, y) }
        DoubleClick -> 
            GraphMap.update 
                (offsetMouse model |> Node.testNode |> GraphMap.AddNode)
                model.graphMap
            |> updateGraphMapHelp model
        Hold ->
            case model.state of
                NoOp ->
                    CmdUtil.noCmd { model | state = MovingCamera model.mousePos model.cameraPos }
                _ ->
                    CmdUtil.noCmd model
        Release ->
            CmdUtil.noCmd { model | state = NoOp }
        Resize size ->
            CmdUtil.noCmd { model | size = size }

updateGraphMapOutMsg: Maybe GraphMap.OutMsg -> Model -> (Model, Cmd Msg)
updateGraphMapOutMsg msg model =
    case msg of
        Just (GraphMap.MouseUp id) ->
            case model.state of
                Connecting id' ->
                    GraphMap.update 
                        (GraphMap.AddEdge id id' {})
                        model.graphMap
                    |> updateGraphMapHelp model
                _ -> CmdUtil.noCmd model
        Just (GraphMap.MouseDown id) ->
            CmdUtil.noCmd { model | state = Connecting id }
        Nothing ->
            (model, Cmd.none)

updateGraphMapHelp: Model -> (GraphMap.Model, Cmd GraphMap.Msg, Maybe GraphMap.OutMsg) -> (Model, Cmd Msg)
updateGraphMapHelp model (graphMap, graphMapcmd, outMsg) =
    let
        (model', cmd) =
            updateGraphMapOutMsg outMsg { model | graphMap = graphMap }
    in
        ( model'
        , Cmd.batch [ cmd, Cmd.map GraphMapMsg graphMapcmd ]
        )


-- VIEW

view: Model -> Html.Html Msg
view model =
    let
        edge =
            case model.state of
                Connecting id ->
                    Just {mousePos = offsetMouse model, originNode = id}
                _ -> Nothing
    in
        Html.div 
            [ unselectableStyle
            , Events.onMouseUp Release
            , Events.onMouseDown Hold
            , Events.onDoubleClick DoubleClick
            , Events.onMouseLeave Release
            , CssUtil.style
                [ Css.width (ipx model.size.width)
                , Css.height (ipx model.size.height)
                ]
            ]
            [ GraphMap.view
                model.size
                model.cameraPos
                edge
                model.graphMap
                |> Html.App.map GraphMapMsg
            ]

unselectableStyle: Html.Attribute msg
unselectableStyle =
    Html.Attributes.style
        [ ("-moz-user-select", "none")
        , ("-webkit-user-select", "none")
        , ("-ms-user-select", "none")
        ]


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (\{x, y} -> Move (x, y))
        , Window.resizes Resize
        , GraphMap.subscriptions model.graphMap |> Sub.map GraphMapMsg
        ]