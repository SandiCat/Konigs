module Node where

import Debug
import Svg
import Signal
import NodeBase
import MetaContent
import Content.Term as Term
import ContentUtil
import Effects exposing (Effects)


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
            let
                (base, fx) = NodeBase.update baseAction model.base
            in
                ( { model | base = base }
                , Effects.map BaseAction fx
                )
        ContentAction contentAction ->
            let
                (content, fx) = MetaContent.update contentAction model.content
            in
                ( { model | content = content }
                , Effects.map ContentAction fx
                )


-- VIEW

type alias Context =
    { mouseActions: Signal.Address NodeBase.MouseAction
    , actions: Signal.Address Action 
    }

view: Context -> Model -> Svg.Svg
view context model =
    Svg.g []
        [ NodeBase.view context.mouseActions model.pos model.radius model.base
        , MetaContent.view
            (Signal.forwardTo context.actions ContentAction
                |> ContentUtil.ViewContext model.pos model.radius)
            model.content
        ]