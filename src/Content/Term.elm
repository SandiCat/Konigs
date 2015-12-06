module Content.Term where

import Svg
import Svg.Attributes as Att
import ContentUtil
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import Signal


-- MODEL

type alias Model =
    { text: String
    }

init: String -> Model
init text =
    Model text


-- UPDATE

type Action
    = InputChange String

update: Action -> Model -> Model
update action model =
    case action of
        InputChange newText ->
            { model | text = newText }


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