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
import List.Extra
import Content.Term.Description as Description
import Html.App
import Option exposing (Option)


-- MODEL

type alias Model =
    { text: String
    , mode: Mode
    , showDescription: Bool
    , description: Description.Model
    , mdl: Material.Model
    }

type Mode
    = Display
    | Input

init: String -> (Model, Cmd Msg)
init text =
    let
        (desc, descCmd) = Description.init "space in the container will be distributed equally to all children. If one of the children has a value of 2, the remaining space would take up twice as much space as the others (or it will try to, at least)."
    in
        Model text Display False desc Material.model ! [ Cmd.map DescriptionMsg descCmd ]

menuOptions: List (Option Msg)
menuOptions =
    [ Option ToggleDescription "description"
    ]


-- UPDATE

type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | EnterInput
    | KeyPress Int
    | DeFocus
    | DescriptionMsg Description.Msg
    | ToggleDescription

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        MdlMsg msg' -> 
            Material.update msg' model
        DescriptionMsg msg' ->
            CmdUtil.update
                (\x -> {model | description = x})
                DescriptionMsg
                (Description.update msg' model.description)
        ToggleDescription ->
            { model | showDescription = not model.showDescription} ! []
        _ ->
            case model.mode of
                Display ->
                    case msg of
                        EnterInput ->
                            CmdUtil.noCmd { model | mode = Input }
                        _ ->
                            CmdUtil.noCmd model
                Input ->
                    case msg of
                        InputChange newText ->
                            CmdUtil.noCmd { model | text = newText }
                        KeyPress code ->
                            if code == 13 then -- 13 is Enter
                                { model
                                    | mode = Display
                                    , text = if model.text == "" then "enter text" else model.text }
                                |> CmdUtil.noCmd
                            else
                                CmdUtil.noCmd model
                        DeFocus ->
                            CmdUtil.noCmd { model | mode = Display }
                        _ ->
                            CmdUtil.noCmd model


-- VIEW

view: (Int, Int) -> Int -> Model -> Html.Html Msg
view pos radius model =
    case model.mode of
        Input ->
            Textfield.render MdlMsg [0] model.mdl
                [ Textfield.onInput InputChange 
                , Textfield.value model.text
                , Textfield.onBlur DeFocus
                ]
        Display ->
            Html.div []
                [ Options.div
                    [ Typo.title
                    , Options.center
                    , MyCss.mdlClass MyCss.TermDisplay
                    ]
                    [ Html.text model.text ]
                , if model.showDescription then
                        Description.view model.description
                        |> Html.App.map DescriptionMsg
                    else
                        Html.div [] []
                ]

onKeyPress: (Int -> msg) -> Html.Attribute msg
onKeyPress tagger =
    Json.Decode.map tagger Events.keyCode
    |> Events.on "onkeypress" 

onDoubleClick: msg -> Html.Attribute msg
onDoubleClick msg =
    Events.onWithOptions
        "ondblclick"
        { stopPropagation = False
        , preventDefault = False
        }
        ( Json.Decode.succeed msg )
