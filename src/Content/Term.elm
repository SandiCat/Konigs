module Content.Term where

import Svg
import Svg.Attributes as Att
import ContentUtil
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import Signal
import Effects exposing (Effects)


-- MODEL

type alias Model =
    { text: String
    }

init: String -> (Model, Effects Action)
init text =
    (Model text, Effects.none)


-- UPDATE

type Action
    = InputChange String

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        InputChange newText ->
            ({ model | text = newText }, Effects.none)


-- VIEW

view: ContentUtil.ViewContext Action -> Model -> Svg.Svg
view context model =
    Svg.foreignObject
        [ Att.width "50"
        , Att.height "50"
        , fst context.pos |> toString |> Att.x
        , snd context.pos |> toString |> Att.y
        ]
        [ Html.input
            [ HtmlAtt.value model.text
            , Events.on "input" Events.targetValue (InputChange >> Signal.message context.actions)
            ]
            []
        ]