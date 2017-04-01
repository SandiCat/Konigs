module Content.Term exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import CmdUtil
import Json.Decode
import MyCss
import Material
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Elevation as Elevation
import List.Extra
import Content.Term.Description as Description
import Option exposing (Option)
import CssUtil
import EventsUtil
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
    [ Option ToggleDescription "description"
    , Option.edit Edit
    ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | Edit
    | KeyPress Keyboard.KeyCode
    | DeFocus
    | DescriptionMsg Description.Msg
    | ToggleDescription
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        DescriptionMsg msg_ ->
            CmdUtil.update
                (\x -> { model | description = x })
                DescriptionMsg
                (Description.update msg_ model.description)

        ToggleDescription ->
            { model | showDescription = not model.showDescription } ! []

        Edit ->
            { model | editing = not model.editing } ! []

        InputChange newText ->
            { model | text = newText } ! []

        KeyPress code ->
            if code == 13 then
                -- 13 is Enter
                { model | editing = False } ! []
            else
                model ! []

        DeFocus ->
            { model | editing = False } ! []

        NoOp ->
            model ! []



-- VIEW


view : ( Int, Int ) -> Int -> Model -> Html.Html Msg
view pos radius model =
    Options.div [ MyCss.mdlClass MyCss.Term ]
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
                Html.text model.text
            ]
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
                    , Options.attribute <| CssUtil.userSelect True
                    , EventsUtil.onMouseDownMdlNoProp NoOp
                    , Textfield.autofocus
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
        , if model.showDescription then
            Description.view model.description
                |> Html.map DescriptionMsg
          else
            Html.div [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress
