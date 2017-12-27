module Content.Term.Description exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import Util.Cmd
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode
import MyCss
import Material
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Material.Icon as Icon
import Util.Css


-- MODEL


type alias Model =
    { mdl : Material.Model
    , text : String
    , mouseIn : Bool
    , editing : Bool
    }


init : String -> ( Model, Cmd Msg )
init text =
    Model Material.model text False False ! []



-- JSON


decode : Decode.Decoder ( Model, Cmd Msg )
decode =
    Decode.succeed init
        |: (Decode.field "text" Decode.string)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "text", Encode.string model.text )
        ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | MouseEnter
    | MouseLeave
    | Edit
    | TextChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseEnter ->
            { model | mouseIn = True } ! []

        MouseLeave ->
            { model | mouseIn = False } ! []

        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        Edit ->
            { model | editing = not model.editing } ! []

        TextChange text ->
            { model | text = text } ! []



-- VIEW


emptyText =
    "No description"


view : Model -> Html.Html Msg
view model =
    Options.div
        [ if model.mouseIn then
            Elevation.e8
          else
            Elevation.e4
        , Elevation.transition 250
        , MyCss.mdlClass MyCss.TermDescription
        , Options.onMouseOver MouseEnter
        , Options.onMouseOut MouseLeave
        , Options.attribute <| Util.Css.userSelect True
        ]
        [ Options.div [ MyCss.mdlClass MyCss.DescriptionText ]
            [ if model.editing then
                Textfield.render MdlMsg
                    [ 1 ]
                    model.mdl
                    [ Typo.body1
                    , Textfield.label emptyText
                    , Textfield.textarea
                    , Textfield.value model.text
                    , Options.onInput TextChange
                    , Textfield.rows 10
                    ]
                    []
              else if model.text == "" then
                Options.div
                    [ Typo.caption
                    , MyCss.mdlClass MyCss.DescriptionEmpty
                    ]
                    [ Html.i [] [ Html.text emptyText ] ]
              else
                Options.div [ Typo.body1 ] [ Html.text model.text ]
            ]
        , if model.mouseIn || model.editing then
            Options.div
                [ MyCss.mdlClass MyCss.DescriptionToolbar ]
                [ Button.render MdlMsg
                    [ 0 ]
                    model.mdl
                    [ Button.fab
                    , Button.colored
                    , Options.onClick Edit
                    ]
                    [ if model.editing then
                        Icon.i "done"
                      else
                        Icon.i "edit"
                    ]
                ]
          else
            Html.div [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
