module MouseManipulator where

import GraphMap exposing (Action (..))
import Time
import Node
import Graph
import Debug
import Html.Attributes
import Html
import Svg
import Svg.Attributes as Att
import FpsClock


-- MODEL

type alias Model =
    { graphMap: GraphMap.Model
    , state: State
    , dtLastClick: Time.Time
    , fpsClock: FpsClock.Model
    }

type State
    = NoOp
    | Connecting Graph.NodeId (Int, Int)

testModel: Model
testModel =
    Model GraphMap.testModel NoOp 10000000 FpsClock.init

getPointedNode: (Int, Int) -> GraphMap.Graph -> Maybe (Graph.NodeId, Node.Model)
getPointedNode pos graph =
    Graph.nodes graph
    |> List.filter (\n -> Node.isMouseWithin pos n.label)
    |> List.map (\n -> (n.id, n.label))
    |> List.head


-- UPDATE

type Action
    = Hold (Int, Int)
    | Release (Int, Int)
    | Move (Int, Int)
    | Tick Time.Time

update: Action -> Model -> Model
update action model =
    case action of
        Hold pos ->
            case model.state of
                NoOp ->
                    startConnecting pos model
                otherwise ->
                    Debug.log "otherwise in Hold/NoOp branch of MouseManipulator" model
        Release pos ->
            case model.state of
                Connecting id pos' ->
                    endConnecting id pos model
                otherwise ->
                    handleDoubleClick pos model
        Move pos ->
            case model.state of
                Connecting id pos' ->
                    {model | state <- Connecting id pos}
                otherwise -> model
        Tick dt ->
            { model
                | graphMap <- GraphMap.update (StepLayout dt) model.graphMap
                , fpsClock <- FpsClock.update dt
                , dtLastClick <- model.dtLastClick + dt
            }

startConnecting: (Int, Int) -> Model -> Model
startConnecting pos model =
    case getPointedNode pos model.graphMap.graph of
        Nothing -> model
        Just (id, node) ->
            {model | state <- Connecting id (fst node.pos, snd node.pos)}

endConnecting: Graph.NodeId -> (Int, Int) -> Model -> Model
endConnecting id pos model =
    case getPointedNode pos model.graphMap.graph of
        Nothing -> {model | state <- NoOp}
        Just (id', node') ->
            if id == id' then
                {model | state <- NoOp}
            else
                { model
                    | graphMap <- GraphMap.update (AddEdge id id' {}) model.graphMap
                    , state <- NoOp
                }

handleDoubleClick: (Int, Int) -> Model -> Model
handleDoubleClick pos model =
    if model.dtLastClick <= 500 then
        { model
            | graphMap <- GraphMap.update (Node.plainNode pos |> AddNode) model.graphMap
            , dtLastClick <- 0
        }
    else
        { model | dtLastClick <- 0 }


-- VIEW

view: (Int, Int) -> Model -> Html.Html
view (w, h) model =
    let
        connection =
            case model.state of
                Connecting id pos ->
                    case Graph.get id model.graphMap.graph of
                        Nothing -> []
                        Just {incoming, node, outgoing} ->
                            [ GraphMap.edgeForm node.label.pos pos ]
                otherwise -> []

        graph =
            [ GraphMap.view model.graphMap ]

        fps =
            [ FpsClock.view model.fpsClock ]

        svg =
            Svg.svg
                [ toString w |> Att.width
                , toString h |> Att.height
                ]
                (connection ++ graph ++ fps)
    in
        Html.div [{-unselectableStyle-}] [svg]

unselectableStyle: Html.Attribute
unselectableStyle =
    Html.Attributes.style
        [ ("-moz-user-select", "none")
        , ("-webkit-user-select", "none")
        , ("-ms-user-select", "none")
        ]
