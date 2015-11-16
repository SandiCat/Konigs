module NodeBase where

import Svg
import Svg.Attributes as Att
import Time
import Easing

-- MODEL

type alias Model =
    { animation: Animation
    }

init: Model
init =
    Appearing 0 |> Model

type Animation
    = None
    | Appearing Time.Time

appearDuration : Float
appearDuration = 0.7 * Time.second


-- UPDATE

type Action
    = Tick Time.Time

update: Action -> Model -> Model
update action model =
    case action of
        Tick dt ->
            case model.animation of
                None -> model
                Appearing elapsed ->
                    let newElapsed = elapsed + dt in
                        if newElapsed >= appearDuration then
                            {model | animation <- None}
                        else
                            {model | animation <- newElapsed |> Appearing}


-- VIEW

view: (Int, Int) -> Int -> Model -> Svg.Svg
view pos radius model =
    let
        radius' =
            case model.animation of
                None -> radius |> toFloat
                Appearing elapsed ->
                    Easing.ease
                        Easing.easeOutBounce
                        Easing.float
                        0
                        (toFloat radius)
                        appearDuration
                        elapsed
    in
        Svg.circle
            [ fst pos |> toString |> Att.cx
            , snd pos |> toString |> Att.cy
            , round radius' |> toString |> Att.r
            , Att.fill "white"
            , Att.stroke "#5E81C1"
            , Att.strokeWidth "7"
            ]
            []