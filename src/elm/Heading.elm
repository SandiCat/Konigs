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
    { text : Maybe String

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


init : String -> ( Model, Cmd Msg )
init text =
    Model (convertText text) Material.model ! []



-- JSON


decode : Decode.Decoder ( Model, Cmd Msg )
decode =
    Decode.succeed init
        |: (Decode.field "text" Decode.string)


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "text", Encode.string (Maybe.withDefault "" model.text) )
        ]



-- UPDATE


type Id
    = Id String


type Msg
    = MdlMsg (Material.Msg Msg)
    | InputChange String
    | OnEnter Id
    | BeginEditing Id
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model

        InputChange newText ->
            { model | text = convertText newText } ! []

        OnEnter (Id id) ->
            model
                ! [ Dom.blur id
                        |> Task.attempt (\_ -> NoOp)
                  ]

        BeginEditing (Id id) ->
            { model | text = Just "" }
                ! [ Dom.focus id
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


view : Int -> Model -> Html.Html Msg
view id model =
    let
        stringId =
            "heading-" ++ toString id
    in
        Options.div
            [ case model.text of
                Nothing ->
                    Typo.caption

                Just _ ->
                    Typo.title
            , Options.center
            , MyCss.mdlClass MyCss.MaxSize
            , if model.text == Nothing then
                Options.onClick <| BeginEditing <| Id stringId
              else
                Options.nop
            ]
            [ case model.text of
                Nothing ->
                    Html.i [] [ Html.text "empty" ]

                Just text ->
                    Html.div
                        [ Att.contenteditable True
                        , MyCss.class [ MyCss.HeadingText ]
                        , onDivBlur InputChange
                        , Util.onEnter <| OnEnter <| Id stringId
                        , Att.id stringId
                        ]
                        [ Html.text text ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
