module Node exposing (..) 

import Svg
import Svg.Attributes as Att
import SvgUtil
import MetaContent
import Content.Term as Term
import CmdUtil
import Html.Events as Events
import Html.App


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , content: MetaContent.MultiModel
    }

init:
    (Int, Int)
    -> (MetaContent.MultiModel, Cmd MetaContent.MultiMsg)
    -> (Model, Cmd Msg)
init pos (content, contentFx) =
    Model pos 40 content ! [ Cmd.map ContentMsg contentFx ]

testNode: (Int, Int) -> (Model, Cmd Msg)
testNode pos =
    let
        (content, fx) = Term.init "Test Term"
    in
        (content |> MetaContent.MTerm, Cmd.map MetaContent.ATerm fx)
        |> init pos


-- UPDATE

type Msg
    = ContentMsg MetaContent.MultiMsg
    | ToParent OutMsg

type OutMsg
    = MouseUp
    | MouseDown

update: Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
update msg model =
    case msg of
        ContentMsg contentMsg ->
            case MetaContent.update contentMsg model.content of
                Just (content, fx) -> 
                    ( { model | content = content }
                    , Cmd.map ContentMsg fx
                    , Nothing
                    )
                Nothing ->
                    ( model, Cmd.none, Nothing )
        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )


-- VIEW

view: Model -> Svg.Svg Msg
view model =
    Svg.g []
        [ Svg.g 
            [ Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            ] 
            [ SvgUtil.circle 7 "#5E81C1" "white" model.pos model.radius ]
        , [ MetaContent.view model.pos model.radius model.content ]
            |> Svg.foreignObject
                [ fst model.pos |> toString |> Att.x
                , snd model.pos |> toString |> Att.y
                ]
            |> Html.App.map ContentMsg
        ]
