module Content.Term exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import Util.Cmd
import Json.Decode as Json
import MyCss
import Material
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Elevation as Elevation
import Content.Term.Description as Description
import Option exposing (Option)
import Util.Css
import Keyboard


-- MODEL


type alias Model =
    { text : String
    , editing : Bool
    , showDescription : Bool
    , description : Description.Model
    , mdl : Material.Model
    }


init : String -> ( Model, Cmd Msg )
init text =
    let
        ( desc, descCmd ) =
            Description.init ""
    in
        Model text False False desc Material.model ! [ Cmd.map DescriptionMsg descCmd ]


menuOptions : List (Option Msg)
menuOptions =
    [ Option ToggleDescription "description" "Toggle description"
    , Option.edit Edit
    ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | Edit
    | DeFocus
    | DescriptionMsg Description.Msg
    | ToggleDescription
    | OnEnter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        DescriptionMsg msg_ ->
            Util.Cmd.update
                (\x -> { model | description = x })
                DescriptionMsg
                (Description.update msg_ model.description)

        ToggleDescription ->
            { model | showDescription = not model.showDescription } ! []

        Edit ->
            { model | editing = not model.editing } ! []

        InputChange newText ->
            { model | text = newText } ! []

        DeFocus ->
            { model | editing = False } ! []

        OnEnter ->
            model ! []



-- VIEW


onDivBlur : (String -> msg) -> Html.Attribute msg
onDivBlur msg =
    -- activates when a contenteditable element has finished editing
    Json.at [ "target", "textContent" ] Json.string
        |> Json.map msg
        |> Events.on "blur"


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        Events.onWithOptions
            "keydown"
            { stopPropagation = False, preventDefault = True }
            (Json.andThen isEnter Events.keyCode)


viewInside : Model -> Html.Html Msg
viewInside model =
    Options.div [ MyCss.mdlClass MyCss.MaxSize ]
        [ Options.div
            [ if model.text == "" then
                Typo.caption
              else
                Typo.title
            , Options.center
            , MyCss.mdlClass MyCss.TermDisplay
            , Options.onDoubleClick Edit
            ]
            [ if model.text == "" then
                Html.i [] [ Html.text "empty" ]
              else
                Html.div
                    [ Att.contenteditable True
                    , MyCss.class [ MyCss.TermText ]
                    , onDivBlur InputChange
                    , onEnter OnEnter
                    ]
                    [ Html.text model.text ]
            ]
        ]


viewOutside : Model -> Html.Html Msg
viewOutside model =
    Options.div [ MyCss.mdlClass MyCss.MaxSize ]
        [ if model.showDescription then
            Description.view model.description
                |> Html.map DescriptionMsg
          else
            Html.div [] []
        , if model.editing then
            Options.div
                [ MyCss.mdlClass MyCss.TermInput
                , Elevation.e4
                ]
                [ Textfield.render MdlMsg
                    [ 0 ]
                    model.mdl
                    [ Options.onInput InputChange
                    , Options.onBlur DeFocus
                    , Textfield.value model.text
                    , Options.attribute <| Util.Css.userSelect True
                    ]
                    []
                , Button.render MdlMsg
                    [ 1 ]
                    model.mdl
                    [ Button.icon
                    , Options.onClick Edit
                    ]
                    [ Icon.i "done" ]
                ]
          else
            Html.div [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
