module Content.Other where

import Svg
import Svg.Attributes as Att
import ContentUtil


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

view: ContentUtil.ViewContext Action -> Model -> Svg.Svg
view context model =
    Svg.text'
        [ fst context.pos |> toString |> Att.x
        , snd context.pos |> toString |> Att.y
        ]
        [ Svg.text "I'm Other" ]