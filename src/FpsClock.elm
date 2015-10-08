module FpsClock where

import Svg
import Svg.Attributes as Att
import Time
import List.Extra


-- MODEL

type alias Model = Time.Time

init: Model
init =
    0


-- UPDATE

update: Time.Time -> Model
update dt =
    dt


-- VIEW

view: Model -> Svg.Svg
view model =
    Time.inSeconds model
    |> (/) 1
    |> round
    |> toString
    |> Svg.text
    |> List.Extra.singleton
    |> Svg.text'
        [ Att.x "10"
        , Att.y "20"
        ]