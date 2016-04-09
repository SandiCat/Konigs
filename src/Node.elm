module Node where

import Svg
import Svg.Attributes as Att
import NodeBase
import MetaContent
import Content.Term as Term
import ContentUtil
import Effects exposing (Effects)
import EffectsUtil


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , base: NodeBase.Model
    , content: MetaContent.MultiModel
    }

init:
    (Int, Int)
    -> (MetaContent.MultiModel, Effects MetaContent.MultiAction)
    -> (Model, Effects Action)
init pos (content, contentFx) =
    let
        (base, baseFx) = NodeBase.init
    in
        (Model pos 40 base content
        , Effects.batch
            [ Effects.map BaseAction baseFx
            , Effects.map ContentAction contentFx
            ]
        )

testNode: (Int, Int) -> (Model, Effects Action)
testNode pos =
    let
        (content, fx) = Term.init "Test Term"
    in
        (content |> MetaContent.MTerm, Effects.map MetaContent.ATerm fx)
        |> init pos


-- UPDATE

type Action
    = BaseAction NodeBase.Action
    | ContentAction MetaContent.MultiAction

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        BaseAction baseAction ->
            EffectsUtil.update
                (\x -> { model | base = x })
                BaseAction
                (NodeBase.update baseAction model.base)
        ContentAction contentAction ->
            case MetaContent.update contentAction model.content of
                Just (content, fx) -> 
                    ( { model | content = content }
                    , Effects.map ContentAction fx
                    )
                Nothing -> EffectsUtil.noFx model


-- VIEW

type alias Context =
    { mouseActions: Signal.Address NodeBase.MouseAction
    , actions: Signal.Address Action
    }

view: Context -> Model -> Svg.Svg
view context model =
    Svg.g []
        [ NodeBase.view context.mouseActions model.pos model.radius model.base
        , [ MetaContent.view
            (Signal.forwardTo context.actions ContentAction
                |> ContentUtil.ViewContext model.pos model.radius)
            model.content ]
            |> Svg.foreignObject
                [ fst model.pos |> toString |> Att.x
                , snd model.pos |> toString |> Att.y
                ]
        ]
