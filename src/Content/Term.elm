module Content.Term exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import CmdUtil
import Json.Decode


-- MODEL

type alias Model =
    { text: String
    , mode: Mode
    }

type Mode
    = Display
    | Input

init: String -> (Model, Cmd Msg)
init text =
    Model text Display |> CmdUtil.noFx


-- UPDATE

type Msg
    = InputChange String
    | EnterInput
    | KeyPress Int
    | DeFocus

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.mode of
        Display ->
            case msg of
                EnterInput ->
                    CmdUtil.noFx { model | mode = Input }
                _ ->
                    CmdUtil.noFx model
        Input ->
            case msg of
                InputChange newText ->
                    CmdUtil.noFx { model | text = newText }
                KeyPress code ->
                    if code == 13 then -- 13 is Enter
                        { model
                            | mode = Display
                            , text = if model.text == "" then "enter text" else model.text }
                        |> CmdUtil.noFx
                    else
                        CmdUtil.noFx model
                DeFocus ->
                    CmdUtil.noFx { model | mode = Display }
                _ ->
                    CmdUtil.noFx model


-- VIEW

view: (Int, Int) -> Int -> Model -> Html.Html Msg
view pos radius model =
    case model.mode of
        Input ->
            Html.input
                [ Att.value model.text
                , Events.onInput InputChange
                , onKeyPress KeyPress
                , Events.onBlur DeFocus
                ]
                []
        Display ->
            Html.div [ Events.onClick EnterInput ] [ Html.text model.text ]

onKeyPress: (Int -> msg) -> Html.Attribute msg
onKeyPress tagger =
    Json.Decode.map tagger Events.keyCode
    |> Events.on "onkeypress" 

onDoubleClick: msg -> Html.Attribute msg
onDoubleClick msg =
    Events.onWithOptions
        "ondblclick"
        { stopPropagation = False
        , preventDefault = False
        }
        ( Json.Decode.succeed msg )
