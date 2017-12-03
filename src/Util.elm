module Util exposing (..)

import Focus exposing (Focus)


nodeFocus : Focus { record | node : field } field
nodeFocus =
    Focus.create .node (\update record -> { record | node = update record.node })


labelFocus : Focus { record | label : field } field
labelFocus =
    Focus.create .label (\update record -> { record | label = update record.label })


type alias Size =
    { width : Int, height : Int }


type alias Option msg =
    { msg : msg
    , icon : String
    , tooltip : String
    }


optionMap : (a -> b) -> Option a -> Option b
optionMap f old =
    { old | msg = f old.msg }
