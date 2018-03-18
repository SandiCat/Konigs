module Main exposing (..)

import Task
import Html
import Util
import Window
import MentalMap
import Util.Cmd
import Material
import Material.Layout
import Material.Button as Button
import Material.Typography as Typo
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.List


-- MODEL


type alias Model =
    { size : Util.Size
    , mentalMap : MentalMap.Model
    { mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( mentalMap, mentalMapCmd ) =
            MentalMap.init
    in
        Model Material.model (Util.Size 0 0) mentalMap
            ! [ Task.perform Resize Window.size
              , Cmd.map MentalMapMsg mentalMapCmd
              ]



-- UPDATE


type Msg
    = Resize Util.Size
    | MentalMapMsg MentalMap.Msg
    | MdlMsg (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

        MentalMapMsg msg_ ->
            Util.Cmd.update
                (\x -> { model | mentalMap = x })
                MentalMapMsg
                (MentalMap.update msg_ model.mentalMap)

        MdlMsg msg_ ->
            Material.update MdlMsg msg_ model



-- VIEW


view : Model -> Html.Html Msg
view model =
    Material.Layout.render MdlMsg
        model.mdl
        []
        { main =
            [ Html.div []
                [ MentalMap.view model.size model.mentalMap
                    |> Html.map MentalMapMsg
                ]
            ]
        , drawer = [ Html.text "hey" ]
        , header = []
        , tabs = ( [], [] )
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , MentalMap.subscriptions model.mentalMap
            |> Sub.map MentalMapMsg
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
