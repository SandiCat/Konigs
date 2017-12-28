module Node exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes
import Color
import Term
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
    , term : Term.Model
    , mouseOver : Bool
    , contextMenu : ContextMenu.Model
    }


init : Int -> String -> Vec2 -> ( Model, Cmd Msg )
init id text pos =
    let
        ( term, termCmd ) =
            Term.init id text
    in
        Model pos 60 term False ContextMenu.init ! [ Cmd.map TermMsg termCmd ]



-- UPDATE


type Msg
    = TermMsg Term.Msg
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
        TermMsg termMsg ->
            let
                ( term, cmd ) =
                    Term.update termMsg model.term
            in
                ( { model | term = term }, Cmd.map TermMsg cmd, Nothing )

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

        Just ContextMenu.ToggleDescription ->
            ( model, Cmd.none, Nothing )

        -- description is to be moved from Term to Node
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
            [ Term.viewOutside model.term
                |> Html.map TermMsg
            , if model.mouseOver || model.contextMenu.mouseOver then
                ContextMenu.view
                    model.contextMenu
                    |> Html.map ContextMenuMsg
              else
                Html.div [] []
            ]
        , Html.div
            [ Events.onMouseEnter MouseOver
            , Events.onMouseLeave MouseOut
            , Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            ]
            [ Term.viewInside model.term
                |> Html.map TermMsg
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


svgView : Model -> Svg Msg
svgView model =
    Svg.circle
        [ SvgAttPx.cx <| Vec2.getX model.pos
        , SvgAttPx.cy <| Vec2.getY model.pos
        , SvgAttPx.r model.radius
        , SvgAtt.fill (SvgTypes.Fill Color.white)
        , SvgAtt.stroke <| Color.rgb 94 129 193
        , SvgAttPx.strokeWidth 7
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Term.subscriptions model.term
        |> Sub.map TermMsg
