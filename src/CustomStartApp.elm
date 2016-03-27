{-
    Copied from the source of elm-startapp.
    Modified to allow for the initial state to depend on some signal.
    (I wanted to use it for the root element's dimensions's dependance on the window's)
    Also changed the view type from Html to Graphics.Element.
-}
module CustomStartApp ( start, Config, App ) where

--import Graphics.Element exposing ( Element )
import Html exposing (Html)
import Task
import Effects exposing (Effects, Never)
import Signal.Extra


type alias Config model action =
    { init : List action -> (model, Effects action)
    , update : action -> model -> (model, Effects action)
    , view : Signal.Address action -> model -> Html
    , inputs : List (Signal.Signal action)
    }

type alias App model =
    { html : Signal Html
    , model : Signal model
    , tasks : Signal (Task.Task Never ())
    }

start: Config model action -> App model
start config =
    let
        singleton action = [ action ]

        -- messages : Signal.Mailbox (List action)
        messages =
            Signal.mailbox []

        -- address : Signal.Address action
        address =
            Signal.forwardTo messages.address singleton

        -- updateStep : action -> (model, Effects action) -> (model, Effects action)
        updateStep action (oldModel, accumulatedEffects) =
            let
                (newModel, additionalEffects) = config.update action oldModel
            in
                (newModel, Effects.batch [accumulatedEffects, additionalEffects])

        -- update : List action -> (model, Effects action) -> (model, Effects action)
        update actions (model, _) =
            List.foldl updateStep (model, Effects.none) actions

        -- inputs : Signal (List action)
        inputs =
            messages.signal :: List.map (Signal.map singleton) config.inputs
            |> listFairMerge
            -- |> Signal.map (Debug.log "in")

        -- effectsAndModel : Signal (model, Effects action)
        effectsAndModel =
            Signal.Extra.foldp' update config.init inputs

        model =
            Signal.map fst effectsAndModel
    in
        { html = Signal.map (config.view address) model
        , model = model
        , tasks = Signal.map (Effects.toTask messages.address << snd) effectsAndModel
        }

listFairMerge: List (Signal (List action)) -> Signal (List action)
listFairMerge =
    List.foldr (Signal.Extra.fairMerge (\l r -> l ++ r)) (Signal.constant [])