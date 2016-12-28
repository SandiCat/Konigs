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
import Platform.Cmd as Cmd
import CmdUtil
import List.Extra


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , radius: Int
    , content: MetaContent.MultiModel
    , mouseOver: Bool
    , contextMenu: ContextMenu.Model
    }

init:
    (Int, Int)
    -> (MetaContent.MultiModel, Cmd MetaContent.MultiMsg)
    -> (Model, Cmd Msg)
init pos (content, contentCmd) =
    Model pos 60 content False ContextMenu.init ! [ Cmd.map ContentMsg contentCmd ]

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
    | MouseOut

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
            let
                (ctxmenu, ctxCmd, ctxOutMsg) =
                    ContextMenu.update menuMsg model.contextMenu

                (model', cmd, outMsg) =
                    updateContextMenu ctxOutMsg model
            in
                ( { model' | contextMenu = ctxmenu }
                , Cmd.batch
                    [ ctxCmd |> Cmd.map ContextMenuMsg
                    , cmd
                    ]
                , outMsg
                )
        MouseOver ->
            ( { model | mouseOver = True }, Cmd.none, Nothing )
        MouseOut ->
            ( { model | mouseOver = False }, Cmd.none, Nothing )

updateContextMenu: Maybe ContextMenu.OutMsg -> Model -> (Model, Cmd Msg, Maybe OutMsg)
updateContextMenu msg model =
    case msg of
        Just ContextMenu.Remove ->
            ( model, Cmd.none, Just Remove )
        Just (ContextMenu.ContentMsg msg) ->
            let
                (content, cmd) =
                    MetaContent.update msg model.content
                    |> Maybe.withDefault (model.content, Cmd.none)
            in
                ( { model | content = content }
                , Cmd.map ContentMsg cmd 
                , Nothing
                )
        _ ->
            ( model, Cmd.none, Nothing )


-- VIEW

width: Model -> Int
width model =
    model.radius * 2 + 7

view: Model -> Html.Html Msg
view model =
    Html.div 
        [ CssUtil.style
            [ fst model.pos |> CssUtil.ipx |> Css.left
            , snd model.pos |> CssUtil.ipx |> Css.top
            , width model |> CssUtil.ipx |> Css.width
            , width model |> CssUtil.ipx |> Css.height
            ]
        , MyCss.class [ MyCss.NodeCont ]
        ]
        [ Html.div
            [ MyCss.class [ MyCss.Node ]
            , Events.onMouseOver MouseOver
            , Events.onMouseOut MouseOut
            , Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            ]
            [ MetaContent.view model.pos model.radius model.content
                |> Html.App.map ContentMsg
            , if model.mouseOver || model.contextMenu.mouseOver then 
                ContextMenu.view
                    (MetaContent.menuOptions model.content)
                    model.contextMenu
                |> Html.App.map ContextMenuMsg
              else
                Html.div [] []
            ]
        ]

baseView: Model -> Svg.Svg Msg
baseView model =
    SvgUtil.circle 7 "#5E81C1" "white" model.pos model.radius 