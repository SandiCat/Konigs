module Content.Button exposing (..)

import Svg
import Svg.Attributes as Att
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import CmdUtil


-- MODEL

type alias Model =
    { counter: Int
    }

init: Int -> (Model, Cmd Msg)
init start =
    Model start |> CmdUtil.noCmd


-- UPDATE

type Msg
    = Increment

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            CmdUtil.noCmd {model | counter = model.counter + 1}

-- VIEW

view: (Int, Int) -> Int -> Model -> Svg.Svg Msg
view pos radius model =
    Svg.foreignObject
        [ Att.width "50"
        , Att.height "50"
        , fst pos |> toString |> Att.x
        , snd pos |> toString |> Att.y
        ]
        [ Html.button 
            [ Events.onClick Increment ] 
            [ toString model.counter |> Html.text]
        ]