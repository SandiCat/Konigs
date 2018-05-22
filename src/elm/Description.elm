module Description exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
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
import Markdown


-- MODEL


type alias Model =
    { mdl : Material.Model
    , text : String
    , mouseIn : Bool
    , editing : Bool
    , maximized : Bool
    , markdown : Html.Html Msg
    }


init : String -> Model
init text =
    { mdl = Material.model
    , text = text
    , mouseIn = False
    , editing = False
    , maximized = False
    , markdown = Markdown.toHtml [] text
    }


changeText : String -> Model -> Model
changeText text model =
    { model
        | text = text

        --, markdown = Markdown.toHtml [] text
    }



-- JSON


decode : Decode.Decoder Model
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
    | ToggleMaximize


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
            changeText text model ! []

        ToggleMaximize ->
            { model | maximized = not model.maximized } ! []



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
        , MyCss.mdlClass MyCss.Description
        , MyCss.mdlClass <|
            if model.maximized then
                MyCss.DescriptionMaximized
            else
                MyCss.DescriptionSmall
        , Options.onMouseOver MouseEnter
        , Options.onMouseOut MouseLeave
        ]
        [ Options.div [ MyCss.mdlClass MyCss.DescriptionContent ]
            [ if model.editing then
                Options.styled_ Html.textarea
                    [ Typo.body1 ]
                    [ MyCss.class [ MyCss.DescriptionEdit ]
                    , Att.defaultValue model.text

                    -- defaultValue instead of value because https://github.com/elm-lang/html/issues/105
                    , Events.onInput TextChange
                    ]
                    []
              else if model.text == "" then
                Options.div
                    [ Typo.caption
                    , MyCss.mdlClass MyCss.DescriptionEmpty
                    ]
                    [ Html.i [] [ Html.text emptyText ] ]
              else
                Options.div
                    [ MyCss.mdlClass MyCss.DescriptionText
                    , Typo.body1
                    ]
                    [ model.markdown ]
            ]
        , if model.mouseIn || model.editing || model.maximized then
            Options.div
                [ MyCss.mdlClass MyCss.DescriptionToolbar ]
                [ Button.render MdlMsg
                    [ 0, 0 ]
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
                , Button.render MdlMsg
                    [ 0, 1 ]
                    model.mdl
                    [ Button.minifab
                    , Options.onClick ToggleMaximize
                    ]
                    [ if model.maximized then
                        Icon.i "fullscreen_exit"
                      else
                        Icon.i "fullscreen"
                    ]
                ]
          else
            Html.div [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
