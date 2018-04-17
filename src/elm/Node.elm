module Node exposing (..)

import TypedSvg as Svg
import TypedSvg.Attributes.InPx as SvgAttPx
import TypedSvg.Attributes as SvgAtt
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types as SvgTypes
import Color
import Heading
import Html.Events as Events
import Html
import MyCss
import Util.Css
import Css
import ContextMenu
import Description
import Platform.Cmd as Cmd
import Math.Vector2 as Vec2 exposing (Vec2)
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode


-- MODEL


type alias Model =
    { pos : Vec2
    , radius : Float
    , showDescription : Bool
    , mouseOver : Bool
    , heading : Heading.Model
    , contextMenu : ContextMenu.Model
    , description : Description.Model
    }


fullInit :
    Vec2
    -> Float
    -> Heading.Model
    -> Description.Model
    -> Model
fullInit pos radius heading desc =
    Model pos radius False False heading ContextMenu.init desc


init : String -> Vec2 -> Model
init text pos =
    fullInit pos 60 (Heading.init text) (Description.init "")



-- JSON


decode : Decode.Decoder Model
decode =
    Decode.succeed (fullInit End)
        |: (Decode.field "pos" Util.decodeVec2)
        |: (Decode.field "radius" Decode.float)
        |: (Decode.field "heading" Heading.decode)
        |: (Decode.field "description" Description.decode)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "pos", Util.encodeVec2 model.pos )
        , ( "radius", Encode.float model.radius )
        , ( "heading", Heading.encode model.heading )
        , ( "description", Description.encode model.description )
        ]



-- UPDATE


type Msg
    = HeadingMsg Heading.Msg
    | ToParent OutMsg
    | ContextMenuMsg ContextMenu.Msg
    | DescriptionMsg Description.Msg
    | MouseOver
    | MouseOut


type OutMsg
    = MouseUp
    | MouseDown
    | Remove


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        HeadingMsg headingMsg ->
            let
                ( heading, cmd ) =
                    Heading.update headingMsg model.heading
            in
                ( { model | heading = heading }, Cmd.map HeadingMsg cmd, Nothing )

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

        DescriptionMsg descMsg ->
            let
                ( desc, descCmd ) =
                    Description.update descMsg model.description
            in
                ( { model | description = desc }
                , Cmd.map DescriptionMsg descCmd
                , Nothing
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
            ( { model | showDescription = not model.showDescription }, Cmd.none, Nothing )

        Nothing ->
            ( model, Cmd.none, Nothing )



-- VIEW


width : Model -> Float
width model =
    model.radius * 2 + 7


view : Int -> Model -> Html.Html Msg
view id model =
    Html.div
        [ MyCss.class [ MyCss.Node ] ]
        [ Html.div []
            [ if model.mouseOver || model.contextMenu.mouseOver then
                ContextMenu.view
                    model.contextMenu
                    |> Html.map ContextMenuMsg
              else
                Html.div [] []
            , if model.showDescription then
                Description.view model.description
                    |> Html.map DescriptionMsg
              else
                Html.div [] []
            ]
        , Html.div
            [ Events.onMouseEnter MouseOver
            , Events.onMouseLeave MouseOut
            , Events.onMouseDown (ToParent MouseDown)
            , Events.onMouseUp (ToParent MouseUp)
            ]
            [ Heading.view id model.heading
                |> Html.map HeadingMsg
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
    Heading.subscriptions model.heading
        |> Sub.map HeadingMsg
