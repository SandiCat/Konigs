module Node where

import Debug
import Svg
import Time
import NodeBase
import MetaContent
import Content.Term as Term


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , base: NodeBase.Model
    , content: MetaContent.MultiModel
    }

init: (Int, Int) -> MetaContent.MultiModel -> Model
init pos content =
    Model pos 40 NodeBase.init content

testNode: (Int, Int) -> Model
testNode pos =
    Term.init "Test Term" |> MetaContent.MTerm
    |> init pos

isMouseWithin: (Int, Int) -> Model -> Bool
isMouseWithin (x, y) model =
    let
        x' = fst model.pos
        y' = snd model.pos
    in
        (x - x')^2 + (y - y')^2 <= model.radius^2


-- UPDATE

type Action
    = Tick Time.Time

update: Action -> Model -> Model
update action model =
    case action of
        Tick dt ->
            {model | base <- NodeBase.update (NodeBase.Tick dt) model.base}


-- VIEW

type alias Context = {}

view: Context -> Model -> Svg.Svg
view context model =
    Svg.g []
        [ NodeBase.view model.pos model.radius model.base
        , MetaContent.view model.pos model.radius model.content
        ]