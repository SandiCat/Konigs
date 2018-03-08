module Main exposing (..)

import Task
import Html
import Util
import Window
import MentalMap
import Util.Cmd


-- MODEL


type alias Model =
    { size : Util.Size
    , mentalMap : MentalMap.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( mentalMap, mentalMapCmd ) =
            MentalMap.init
    in
        Model (Util.Size 0 0) mentalMap
            ! [ Task.perform Resize Window.size
              , mentalMapCmd |> Cmd.map MentalMapMsg
              ]



-- UPDATE


type Msg
    = Resize Util.Size
    | MentalMapMsg MentalMap.Msg


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



-- VIEW


view : Model -> Html.Html Msg
view model =
    MentalMap.view model.size model.mentalMap
        |> Html.map MentalMapMsg



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
