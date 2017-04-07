module Content.Button exposing (..)

import Svg
import Svg.Attributes as Att
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import CmdUtil


-- MODEL


type alias Model =
    { counter : Int
    }


init : Int -> ( Model, Cmd Msg )
init start =
    Model start ! []


menuOptions =
    []



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 } ! []



-- VIEW


viewInside : ( Int, Int ) -> Int -> Model -> Svg.Svg Msg
viewInside pos radius model =
    Svg.foreignObject
        [ Att.width "50"
        , Att.height "50"
        , Tuple.first pos |> toString |> Att.x
        , Tuple.second pos |> toString |> Att.y
        ]
        [ Html.button
            [ Events.onClick Increment ]
            [ toString model.counter |> Html.text ]
        ]


viewOutside : ( Int, Int ) -> Int -> Model -> Svg.Svg Msg
viewOutside pos radius model =
    Svg.g [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
