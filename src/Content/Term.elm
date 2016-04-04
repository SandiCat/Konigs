module Content.Term where

import ContentUtil
import Html
import Html.Attributes as Att
import Html.Events as Events
import Signal
import Effects exposing (Effects)
import EffectsUtil
import Json.Decode


-- MODEL

type alias Model =
    { text: String
    , mode: Mode
    }

type Mode
    = Display
    | Input

init: String -> (Model, Effects Action)
init text =
    Model text Display |> EffectsUtil.noFx


-- UPDATE

type Action
    = InputChange String
    | EnterInput
    | KeyPress Int
    | DeFocus

update: Action -> Model -> (Model, Effects Action)
update action model =
    case model.mode of
        Display ->
            case action of
                EnterInput ->
                    EffectsUtil.noFx { model | mode = Input }
                _ ->
                    EffectsUtil.noFx model
        Input ->
            case action of
                InputChange newText ->
                    EffectsUtil.noFx { model | text = newText }
                KeyPress code ->
                    if code == 13 then -- 13 is Enter
                        { model
                            | mode = Display
                            , text = if model.text == "" then "enter text" else model.text }
                        |> EffectsUtil.noFx
                    else
                        EffectsUtil.noFx model
                DeFocus ->
                    EffectsUtil.noFx { model | mode = Display }
                _ ->
                    EffectsUtil.noFx model


-- VIEW

view: ContentUtil.ViewContext Action -> Model -> Html.Html
view context model =
    case model.mode of
        Input ->
            Html.input
                [ Att.value model.text
                , Events.on
                    "input"
                    Events.targetValue
                    (InputChange >> Signal.message context.actions)
                , Events.onKeyPress context.actions KeyPress
                , Events.onBlur context.actions DeFocus
                ]
                []
        Display ->
            Html.div [ Events.onClick context.actions EnterInput ] [ Html.text model.text ]

onDoubleClick: Signal.Address a -> a -> Html.Attribute
onDoubleClick address action =
    Events.onWithOptions
        "ondblclick"
        { stopPropagation = False
        , preventDefault = False
        }
        ( Json.Decode.value )
        ( \_ -> Signal.message address action |> Debug.log "doubleclick" )
