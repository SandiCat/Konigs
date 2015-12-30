module Content.Term where

import Svg
import Svg.Attributes as Att
import SvgHelp
import ContentUtil
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import Signal
import Effects exposing (Effects)
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
    (Model text Display, Effects.none)


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
                    ( { model | mode = Input }
                    , Effects.none
                    )
                _ ->
                    ( model, Effects.none )
        Input ->
            case action of
                InputChange newText ->
                    ( { model | text = newText }
                    , Effects.none
                    )
                KeyPress code ->
                    if code == 13 then -- 13 is Enter
                        ( { model 
                            | mode = Display
                            , text = if model.text == "" then "enter text" else model.text }
                        , Effects.none
                        )
                    else
                        ( model, Effects.none )
                DeFocus ->
                    ( { model | mode = Display }
                    , Effects.none
                    )
                _ ->
                    ( model, Effects.none)


-- VIEW

view: ContentUtil.ViewContext Action -> Model -> Svg.Svg
view context model =
    case model.mode of
        Input ->
            Svg.foreignObject
                [ Att.width "50"
                , Att.height "50"
                , fst context.pos |> toString |> Att.x
                , snd context.pos |> toString |> Att.y
                ]
                [ Html.input
                    [ HtmlAtt.value model.text
                    , Events.on 
                        "input"
                        Events.targetValue
                        (InputChange >> Signal.message context.actions)
                    , Events.onKeyPress context.actions KeyPress
                    , Events.onBlur context.actions DeFocus
                    ]
                    []
                ]
        Display ->
            Svg.text'
                (
                    ( SvgHelp.position context.pos )
                    ++
                    [ Events.onClick context.actions EnterInput ]
                )
                [ Svg.text model.text ]

onDoubleClick: Signal.Address a -> a -> Html.Attribute
onDoubleClick address action =
    Events.onWithOptions
        "ondblclick"
        { stopPropagation = False
        , preventDefault = False
        }
        ( Json.Decode.value )
        ( \_ -> Signal.message address action |> Debug.log "doubleclick" )