module NodeBase where

import Time
import Easing
import SvgUtil
import Svg
import Svg.Attributes as Att
import Html
import Html.Events as Events
import Effects exposing (Effects)
import EffectsUtil


-- MODEL

type alias Model =
    { animation: Animation
    , prevTime: Time.Time
    , elapsed: Time.Time
    }

init: (Model, Effects Action)
init =
    (Model (Begin Appearing) 0 0, Effects.tick Tick)

type Animation
    = None
    | Appearing
    | Begin Animation

duration : Float
duration = 0.7 * Time.second


-- UPDATE

type Action
    = Tick Time.Time

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Tick time ->
            case model.animation of
                None -> init
                Begin animation ->
                    ( Model animation time 0
                    , Effects.tick Tick
                    )
                _ ->
                    if model.elapsed + time - model.prevTime >= duration then
                        Model None 0 0 |> EffectsUtil.noFx
                    else
                        ( { model |
                                elapsed = model.elapsed + time - model.prevTime,
                                prevTime = time }
                        , Effects.tick Tick
                        )


-- VIEW

type MouseAction
    = Down
    | Up

view: Signal.Address MouseAction -> (Int, Int) -> Int -> Model -> Svg.Svg
view mouseAddress pos radius model =
    let
        radius' =
            case model.animation of
                Appearing ->
                    Easing.ease
                        Easing.easeOutBounce
                        Easing.float
                        0
                        (toFloat radius)
                        duration
                        model.elapsed
                Begin _ ->
                    0
                None ->
                    radius |> toFloat
    in
        Svg.g 
            [ Events.onMouseDown mouseAddress Down
            , Events.onMouseUp mouseAddress Up
            ] 
            [ SvgUtil.circle pos (round radius') 7 "#5E81C1" "white" ]
