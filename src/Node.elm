module Node exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import Color
import MetaContent
import Content.Term as Term
import Html.Events as Events
import Html
import MyCss
import Util.Css
import Css
import ContextMenu
import Platform.Cmd as Cmd
import Util.Cmd
import Math.Vector2 as Vec2 exposing (Vec2)


-- MODEL


type alias Model =
    { pos : Vec2
    , radius : Float
    , content : MetaContent.MultiModel
    , mouseOver : Bool
    , contextMenu : ContextMenu.Model
    }


init :
    Vec2
    -> ( MetaContent.MultiModel, Cmd MetaContent.MultiMsg )
    -> ( Model, Cmd Msg )
init pos ( content, contentCmd ) =
    Model pos 60 content False ContextMenu.init ! [ Cmd.map ContentMsg contentCmd ]


testNode : Int -> Vec2 -> ( Model, Cmd Msg )
testNode id pos =
    let
        ( content, cmd ) =
            Term.init id "Test Term"
    in
        ( content |> MetaContent.MdlTerm, Cmd.map MetaContent.MsgTerm cmd )
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


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        ContentMsg contentMsg ->
            case MetaContent.update contentMsg model.content of
                Just ( content, cmd ) ->
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
                ( ctxmenu, ctxCmd, ctxOutMsg ) =
                    ContextMenu.update menuMsg model.contextMenu

                ( model_, cmd, outMsg ) =
                    updateContextMenu ctxOutMsg model
            in
                ( { model_ | contextMenu = ctxmenu }
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


updateContextMenu : Maybe ContextMenu.OutMsg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateContextMenu msg model =
    case msg of
        Just ContextMenu.Remove ->
            ( model, Cmd.none, Just Remove )

        Just (ContextMenu.ContentMsg msg) ->
            let
                ( content, cmd ) =
                    MetaContent.update msg model.content
                        |> Maybe.withDefault ( model.content, Cmd.none )
            in
                ( { model | content = content }
                , Cmd.map ContentMsg cmd
                , Nothing
                )

        Nothing ->
            ( model, Cmd.none, Nothing )



-- VIEW


width : Model -> Float
width model =
    model.radius * 2 + 7


view : Model -> Html.Html Msg
view model =
    Html.div
        [ MyCss.class [ MyCss.Node ] ]
        [ Html.div []
            [ MetaContent.viewOutside model.content
                |> Html.map ContentMsg
            , if model.mouseOver || model.contextMenu.mouseOver then
                ContextMenu.view
                    (MetaContent.menuOptions model.content)
                    model.contextMenu
                    |> Html.map ContextMenuMsg
              else
                Html.div [] []
            ]
        , Html.div
            [ Events.onMouseOver MouseOver
            , Events.onMouseOut MouseOut
            , Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            ]
            [ MetaContent.viewInside model.content
                |> Html.map ContentMsg
            ]
        ]
        |> List.singleton
        |> Html.div
            [ Util.Css.style
                [ Vec2.getX model.pos |> Css.px |> Css.left
                , Vec2.getY model.pos |> Css.px |> Css.top
                , width model |> Css.px |> Css.width
                , width model |> Css.px |> Css.height
                ]
            , MyCss.class [ MyCss.NodeCont ]

            {- Node and NodeCont cannot be merged. Node has to have a parent with
               it's width and height so that it can be centered using `top: 50%` and
               `left: 50%`, without `transform`.
            -}
            ]


baseView : Model -> Svg Msg
baseView model =
    Svg.circle
        [ SvgAttPx.cx <| Vec2.getX model.pos
        , SvgAttPx.cy <| Vec2.getY model.pos
        , SvgAttPx.r model.radius
        , SvgAtt.fill Color.white
        , SvgAtt.stroke <| Color.rgb 94 129 193
        , SvgAttPx.strokeWidth 7
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ContentMsg (MetaContent.subscriptions model.content)
