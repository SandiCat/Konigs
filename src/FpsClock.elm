module FpsClock where

import Svg
import Svg.Attributes as Att
import Time
import List.Extra
import Effects exposing (Effects)


-- MODEL

type alias Model =
    { prevTime: Time.Time
    , dt: Time.Time
    }

init: (Model, Effects Action)
init =
    (Model 0 0, Effects.tick Tick)


-- UPDATE

type Action
    = Tick Time.Time

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Tick time ->
            ( Model time (time - model.prevTime)
            , Effects.tick Tick
            )


-- VIEW

view: Model -> Svg.Svg
view model =
    Time.inSeconds model.dt
    |> (/) 1
    |> round
    |> toString
    |> Svg.text
    |> List.Extra.singleton
    |> Svg.text'
        [ Att.x "10"
        , Att.y "20"
        ]