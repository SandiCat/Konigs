module MouseManipulator exposing (init, update, view, subscriptions)

import GraphMap
import Node
import Graph
import Html.Attributes
import Html
import Html.Events as Events
import CmdUtil
import Css exposing (px)
import CssUtil exposing (ipx)
import Mouse
import Window
import Task
import Math.Vector2 as Vec2 exposing (Vec2)


-- MODEL


type alias Model =
    { graphMap : GraphMap.Model
    , state : State
    , mousePos : Vec2
    , size : { width : Int, height : Int }
    , cameraPos : Vec2
    }


type State
    = NoOp
    | Connecting Graph.NodeId
    | MovingCamera Vec2 Vec2


offsetMouse : Model -> Vec2
offsetMouse model =
    Vec2.sub model.mousePos model.cameraPos


init : ( Model, Cmd Msg )
init =
    let
        ( graphMap, gmCmd ) =
            GraphMap.init
    in
        Model graphMap NoOp (Vec2.vec2 0 0) { width = 0, height = 0 } (Vec2.vec2 0 0)
            ! [ gmCmd |> Cmd.map GraphMapMsg
              , Task.perform Resize Window.size
              ]



-- UPDATE


type Msg
    = Move Vec2
    | GraphMapMsg GraphMap.Msg
    | Resize { width : Int, height : Int }
    | LeaveWindow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GraphMapMsg graphMapMsg ->
            GraphMap.update graphMapMsg model.graphMap
                |> updateGraphMapHelp model

        Move newMousePos ->
            case model.state of
                MovingCamera mousePos cameraPos ->
                    { model | cameraPos = Vec2.sub (Vec2.add cameraPos newMousePos) mousePos } ! []

                _ ->
                    { model | mousePos = newMousePos } ! []

        Resize size ->
            { model | size = size } ! []

        LeaveWindow ->
            { model | state = NoOp } ! []


updateGraphMapOutMsg : Maybe GraphMap.OutMsg -> Model -> ( Model, Cmd Msg )
updateGraphMapOutMsg msg model =
    case msg of
        Just (GraphMap.MouseUp id) ->
            case model.state of
                Connecting id_ ->
                    GraphMap.update
                        (GraphMap.AddEdge id id_ {})
                        model.graphMap
                        |> updateGraphMapHelp { model | state = NoOp }

                _ ->
                    model ! []

        Just (GraphMap.MouseDown id) ->
            { model | state = Connecting id } ! []

        Just (GraphMap.Doubleclick) ->
            GraphMap.update
                (offsetMouse model |> Node.testNode |> GraphMap.AddNode)
                model.graphMap
                |> updateGraphMapHelp model

        Just (GraphMap.Hold) ->
            case model.state of
                NoOp ->
                    { model | state = MovingCamera model.mousePos model.cameraPos } ! []

                _ ->
                    model ! []

        Just (GraphMap.Release) ->
            { model | state = NoOp } ! []

        Nothing ->
            ( model, Cmd.none )


updateGraphMapHelp : Model -> ( GraphMap.Model, Cmd GraphMap.Msg, Maybe GraphMap.OutMsg ) -> ( Model, Cmd Msg )
updateGraphMapHelp model ( graphMap, graphMapcmd, outMsg ) =
    let
        ( model_, cmd ) =
            updateGraphMapOutMsg outMsg { model | graphMap = graphMap }
    in
        ( model_
        , Cmd.batch [ cmd, Cmd.map GraphMapMsg graphMapcmd ]
        )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        edge =
            case model.state of
                Connecting id ->
                    Just { mousePos = offsetMouse model, originNode = id }

                _ ->
                    Nothing
    in
        Html.div
            [ CssUtil.userSelect False
            , Events.onMouseLeave LeaveWindow
            ]
            [ GraphMap.view
                model.size
                model.cameraPos
                edge
                model.graphMap
                |> Html.map GraphMapMsg
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (\{ x, y } -> Move <| Vec2.vec2 (toFloat x) (toFloat y))
        , Window.resizes Resize
        , GraphMap.subscriptions model.graphMap |> Sub.map GraphMapMsg
        ]
