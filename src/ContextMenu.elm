module ContextMenu exposing (..)

import Html
import Html.Events as Events
import MyCss
import MetaContent
import Option exposing (Option)
import Material.Button as Button
import Material.Icon as Icon
import Material
import Material.Options as Options
import EventsUtil


-- MODEL


type alias Model =
    { mdl : Material.Model
    , mouseOver : Bool
    }


init : Model
init =
    Model Material.model False



-- UPDATE


type Msg
    = ToParent OutMsg
    | MouseOver
    | MouseOut
    | NoOp
    | MdlMsg (Material.Msg Msg)


type OutMsg
    = Remove
    | Edit
    | ContentMsg MetaContent.MultiMsg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        ToParent outMsg ->
            ( model, Cmd.none, Just outMsg )

        MouseOver ->
            ( { model | mouseOver = True }, Cmd.none, Nothing )

        MouseOut ->
            ( { model | mouseOver = False }, Cmd.none, Nothing )

        MdlMsg msg_ ->
            let
                ( model_, cmd ) =
                    Material.update MdlMsg msg_ model
            in
                ( model_, cmd, Nothing )

        NoOp ->
            ( model, Cmd.none, Nothing )



-- VIEW


baseOptions : List (Option OutMsg)
baseOptions =
    [ Option Remove "delete"
    , Option Edit "edit"
    ]


view : List (Option MetaContent.MultiMsg) -> Model -> Html.Html Msg
view options model =
    let
        options_ =
            List.map (Option.map ContentMsg) options
    in
        List.map (optionView model) (options_ ++ baseOptions)
            |> Html.div
                [ MyCss.class [ MyCss.ContextMenu ]
                , Events.onMouseEnter MouseOver
                , Events.onMouseLeave MouseOut
                ]


optionView : Model -> Option OutMsg -> Html.Html Msg
optionView model option =
    Button.render MdlMsg
        [ 0 ]
        model.mdl
        [ Button.icon
        , EventsUtil.onClickMdlNoProp (ToParent option.msg)
        , EventsUtil.onMouseDownMdlNoProp NoOp
        ]
        [ Icon.i option.icon ]
