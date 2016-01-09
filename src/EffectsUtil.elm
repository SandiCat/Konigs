module EffectsUtil where

import Effects exposing (Effects)


noFx: a -> (a, Effects b)
noFx model =
    (model, Effects.none)

update:
    (inModel -> outModel)
    -> (inAction -> outAction)
    -> (inModel, Effects inAction)
    -> (outModel, Effects outAction)
update updateOutModel tagAction (inModel, inFx) =
    (updateOutModel inModel, Effects.map tagAction inFx)