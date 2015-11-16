module Node
    (Model, view, Context, isMouseWithin, init, update, Action (..))
    where

import Debug
import Svg
import Svg.Attributes as Att
import Time
import NodeBase exposing (Action(..))


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , base: NodeBase.Model
    }

init: (Int, Int) -> Model
init pos =
    Model pos 40 NodeBase.init

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
    NodeBase.view model.pos model.radius model.base