module CmdUtil exposing (..)


update :
    (inModel -> outModel)
    -> (inMsg -> outMsg)
    -> ( inModel, Cmd inMsg )
    -> ( outModel, Cmd outMsg )
update updateOutModel tagMsg ( inModel, inCmd ) =
    ( updateOutModel inModel, Cmd.map tagMsg inCmd )
