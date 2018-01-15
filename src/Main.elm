module Main exposing (..)

import Task
import Html
import Util
import Window
import GraphMap
import Util.Cmd


-- MODEL


type alias Model =
    { size : Util.Size
    , graphMap : GraphMap.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( graphMap, graphMapCmd ) =
            GraphMap.init
    in
        Model (Util.Size 0 0) graphMap
            ! [ Task.perform Resize Window.size
              , graphMapCmd |> Cmd.map GraphMapMsg
              ]



-- UPDATE


type Msg
    = Resize Util.Size
    | GraphMapMsg GraphMap.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

        GraphMapMsg msg_ ->
            Util.Cmd.update
                (\x -> { model | graphMap = x })
                GraphMapMsg
                (GraphMap.update msg_ model.graphMap)



-- VIEW


view : Model -> Html.Html Msg
view model =
    GraphMap.view model.size model.graphMap
        |> Html.map GraphMapMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , GraphMap.subscriptions model.graphMap
            |> Sub.map GraphMapMsg
        ]



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
