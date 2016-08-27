module Node exposing (..) 

import Svg
import SvgUtil
import MetaContent
import Content.Term as Term
import Html.Events as Events
import Html.App
import Html
import MyCss
import CssUtil
import List.Extra


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
init pos (content, contentCmd) =
    Model pos 40 content ! [ Cmd.map ContentMsg contentCmd ]

testNode: (Int, Int) -> (Model, Cmd Msg)
testNode pos =
    let
        (content, cmd) = Term.init "Test Term"
    in
        (content |> MetaContent.MdlTerm, Cmd.map MetaContent.MsgTerm cmd)
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
                Just (content, cmd) -> 
                    ( { model | content = content }
                    , Cmd.map ContentMsg cmd
                    , Nothing
                    )
                Nothing ->
                    ( model, Cmd.none, Nothing )
        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )


-- VIEW

view: Model -> Html.Html Msg
view model =
    MetaContent.view model.pos model.radius model.content
    |> Html.App.map ContentMsg
    |> List.Extra.singleton
    |> Html.div
        [ MyCss.class [ MyCss.Node ]
        , CssUtil.position model.pos
        ]

baseView: Model -> Svg.Svg Msg
baseView model =
    Svg.g 
        [ Events.onMouseDown (ToParent MouseDown)
        , Events.onMouseUp (ToParent MouseUp)
        ] 
        [ SvgUtil.circle 7 "#5E81C1" "white" model.pos model.radius ]