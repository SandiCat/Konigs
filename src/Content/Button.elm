module Content.Button where

import Svg
import Svg.Attributes as Att
import ContentUtil
import Html
import Html.Attributes as HtmlAtt
import Html.Events as Events
import Effects exposing (Effects)
import EffectsUtil


-- MODEL

type alias Model =
    { counter: Int
    }

init: Int -> (Model, Effects Action)
init start =
    Model start |> EffectsUtil.noFx


-- UPDATE

type Action
    = Increment

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Increment ->
            EffectsUtil.noFx {model | counter = model.counter + 1}

-- VIEW

view: ContentUtil.ViewContext Action -> Model -> Svg.Svg
view context model =
    Svg.foreignObject
        [ Att.width "50"
        , Att.height "50"
        , fst context.pos |> toString |> Att.x
        , snd context.pos |> toString |> Att.y
        ]
        [ Html.button 
            [ Events.onClick context.actions Increment ] 
            [ toString model.counter |> Html.text]
        ]