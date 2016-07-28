module CmdUtil exposing (..) 

noFx: a -> (a, Cmd b)
noFx model =
    (model, Cmd.none)

update:
    (inModel -> outModel)
    -> (inMsg -> outMsg)
    -> (inModel, Cmd inMsg)
    -> (outModel, Cmd outMsg)
update updateOutModel tagMsg (inModel, inFx) =
    (updateOutModel inModel, Cmd.map tagMsg inFx)