module Content.Term.Description exposing (..)

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
import Material.Elevation as Elevation


-- MODEL


type alias Model =
    { mdl : Material.Model
    , text : String
    }


init : String -> ( Model, Cmd Msg )
init text =
    Model Material.model text |> CmdUtil.noCmd



-- UPDATE


type Msg
    = MdlMsg (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MdlMsg msg_ ->
            Material.update msg_ model



-- VIEW


view : Model -> Html.Html Msg
view model =
    Options.div
        [ Elevation.e4
        , MyCss.mdlClass MyCss.TermDescription
        , Options.when Options.center (model.text == "")
        , Typo.body1
        ]
        [ Html.text model.text ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
