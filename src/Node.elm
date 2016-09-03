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
import Css
import ContextMenu


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , content: MetaContent.MultiModel
    , state: State
    }

type State
    = NoOp
    | DisplayingMenu

init:
    (Int, Int)
    -> (MetaContent.MultiModel, Cmd MetaContent.MultiMsg)
    -> (Model, Cmd Msg)
init pos (content, contentCmd) =
    Model pos 40 content NoOp ! [ Cmd.map ContentMsg contentCmd ]

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
    | ContextMenuMsg ContextMenu.Msg
    | MouseOver
    | MouseLeave

type OutMsg
    = MouseUp
    | MouseDown
    | Remove

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
        ContextMenuMsg menuMsg ->
            updateContextMenu menuMsg model
        MouseOver ->
            ( { model | state = DisplayingMenu }, Cmd.none, Nothing )
        MouseLeave ->
            ( { model | state = NoOp }, Cmd.none, Nothing )

updateContextMenu: ContextMenu.Msg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
updateContextMenu msg model =
    case msg of
        ContextMenu.Remove ->
            ( model, Cmd.none, Just Remove )
        _ ->
            ( model, Cmd.none, Nothing )


-- VIEW

view: Model -> Html.Html Msg
view model =
    Html.div
        [ MyCss.class [ MyCss.Node ]
        , CssUtil.position model.pos
        ]
        [ MetaContent.view model.pos model.radius model.content
            |> Html.App.map ContentMsg
        , Html.div
            [ Events.onMouseOver MouseOver
            , Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            , MyCss.class [ MyCss.NodeOver ]
            , CssUtil.style
                [ model.radius * 2 |> CssUtil.ipx |> Css.width
                , model.radius * 2 |> CssUtil.ipx |> Css.height 
                ]
            ]
            [ Html.div
                [ Events.onMouseOut MouseLeave
                , MyCss.class [ MyCss.NodeLeave ]
                ]
                [ if model.state == DisplayingMenu then 
                    ContextMenu.view |> Html.App.map ContextMenuMsg
                  else
                    Html.div [] []
                ]
            ]
        ]

baseView: Model -> Svg.Svg Msg
baseView model =
    Svg.g []
        [ SvgUtil.circle 7 "#5E81C1" "white" model.pos model.radius ]