module Heading exposing (..)

import Html
import Html.Attributes as Att
import Html.Events as Events
import Util.Cmd
import MyCss
import Material
import Material.Typography as Typo
import Material.Options as Options
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
    , mdl : Material.Model
    }


convertText : String -> Maybe String
convertText text =
    if text == "" then
        Nothing
    else
        Just text


init : Int -> String -> ( Model, Cmd Msg )
init id text =
    Model ("heading-input-" ++ toString id) (convertText text) Material.model ! []



-- JSON


decode : Int -> Decode.Decoder ( Model, Cmd Msg )
decode id =
    Decode.succeed (init id)
        |: (Decode.field "text" Decode.string)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "text", Encode.string (Maybe.withDefault "" model.text) )
        ]



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | OnEnter
    | BeginEditing
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

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


view : Model -> Html.Html Msg
view model =
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
