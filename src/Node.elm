module Node
    (Model, view, Context, isMouseWithin, init, update, Action (..))
    where

import Debug
import Svg
import Svg.Attributes as Att
import Time
import Easing


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , text: String
    , radius: Int
    , thickness: Int
    , animation: Animation
    }

type Animation
    = None
    | Appearing Time.Time

init: (Int, Int) -> Model
init pos =
    Model pos "" 40 7 (Appearing 0)

isMouseWithin: (Int, Int) -> Model -> Bool
isMouseWithin (x, y) model =
    let
        x' = fst model.pos
        y' = snd model.pos
    in
        (x - x')^2 + (y - y')^2 <= model.radius^2

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

type alias Context = {}

view: Context -> Model -> Svg.Svg
view context model =
    let
        radius' =
            case model.animation of
                None -> model.radius |> toFloat
                Appearing elapsed ->
                    Easing.ease
                        Easing.easeOutBounce
                        Easing.float
                        0
                        (toFloat model.radius)
                        appearDuration
                        elapsed
    in
        Svg.g []
            [ Svg.circle
                [ fst model.pos |> toString |> Att.cx
                , snd model.pos |> toString |> Att.cy
                , round radius' |> toString |> Att.r
                , Att.fill "white"
                , Att.stroke "blue"
                , toString model.thickness |> Att.strokeWidth
                ]
                []
            , Svg.text'
                [ fst model.pos |> toString |> Att.x
                , snd model.pos |> toString |> Att.y
                , Att.textAnchor "middle"
                ]
                [ Svg.text model.text ]
            ]
