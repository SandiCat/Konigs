module Content.Other where

import Svg
import Svg.Attributes as Att


-- MODEL

type alias Model =
    { text: String
    }

init: String -> Model
init text =
    Model text


-- UPDATE

type Action
    = NoOp

update: Action -> Model -> Model
update action model =
    model

-- VIEW

view: (Int, Int) -> Int -> Model -> Svg.Svg
view pos radius model =
    Svg.text'
        [ fst pos |> toString |> Att.x
        , snd pos |> toString |> Att.y
        ]
        [ Svg.text "I'm Other" ]