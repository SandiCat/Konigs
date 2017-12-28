module Heading exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import Util.Cmd
import MyCss
import Material
import Material.Typography as Typo
import Material.Options as Options
import Description
import Util
import Util.Css
import Dom
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode.Extra
import Json.Encode as Encode
import Task


-- MODEL


type alias Model =
    { inputId : String
    , text : Maybe String

    {- Text is Nothing when it's purposefully empty. Placeholder text will be
       displayed in the view with proper styling. However, text can also be `Just ""`.
       This is used when a previously empty Heading is being edited. In this case, HeadingText
       should be in focus. If a still-empty HeadingText blurs, text should become Nothing.
    -}
    , showDescription : Bool
    , description : Description.Model
    , mdl : Material.Model
    }


convertText : String -> Maybe String
convertText text =
    if text == "" then
        Nothing
    else
        Just text


fullInit : Int -> String -> ( Description.Model, Cmd Description.Msg ) -> ( Model, Cmd Msg )
fullInit id text ( desc, descCmd ) =
    Model ("heading-input-" ++ toString id) (convertText text) False desc Material.model
        ! [ Cmd.map DescriptionMsg descCmd ]


init : Int -> String -> ( Model, Cmd Msg )
init id text =
    fullInit id text (Description.init "")


menuOptions : List (Util.Option Msg)
menuOptions =
    [ Util.Option ToggleDescription "description" "Toggle description"
    ]



-- JSON


decode : Int -> Decode.Decoder ( Model, Cmd Msg )
decode id =
    Decode.succeed (fullInit id)
        |: (Decode.field "text" Decode.string)
        |: (Decode.field "description" Description.decode)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "text", Json.Encode.Extra.maybe Encode.string model.text )
        , ( "description", Description.encode model.description )
        ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | DescriptionMsg Description.Msg
    | ToggleDescription
    | OnEnter
    | BeginEditing
    | NoOp


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

        InputChange newText ->
            { model | text = convertText newText } ! []

        OnEnter ->
            model
                ! [ Dom.blur model.inputId
                        |> Task.attempt (\_ -> NoOp)
                  ]

        BeginEditing ->
            { model | text = Just "" }
                ! [ Dom.focus model.inputId
                        |> Task.attempt (\_ -> NoOp)
                  ]

        NoOp ->
            model ! []



-- VIEW


onDivBlur : (String -> msg) -> Html.Attribute msg
onDivBlur msg =
    -- activates when a contenteditable element has finished editing
    Decode.at [ "target", "textContent" ] Decode.string
        |> Decode.map msg
        |> Events.on "blur"


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        Events.onWithOptions
            "keydown"
            { stopPropagation = False, preventDefault = True }
            (Decode.andThen isEnter Events.keyCode)


viewInside : Model -> Html.Html Msg
viewInside model =
    Options.div
        [ case model.text of
            Nothing ->
                Typo.caption

            Just _ ->
                Typo.title
        , Options.center
        , MyCss.mdlClass MyCss.MaxSize
        ]
        [ case model.text of
            Nothing ->
                Html.i
                    [ Events.onClick BeginEditing ]
                    [ Html.text "empty" ]

            Just text ->
                Html.div
                    [ Att.contenteditable True
                    , MyCss.class [ MyCss.HeadingText ]
                    , onDivBlur InputChange
                    , onEnter OnEnter
                    , Att.id model.inputId
                    ]
                    [ Html.text text ]
        ]


viewOutside : Model -> Html.Html Msg
viewOutside model =
    Options.div [ MyCss.mdlClass MyCss.MaxSize ]
        [ if model.showDescription then
            Description.view model.description
                |> Html.map DescriptionMsg
          else
            Html.div [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
