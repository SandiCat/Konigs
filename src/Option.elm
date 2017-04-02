module Option exposing (..)


type alias Option msg =
    { msg : msg
    , icon : String
    , tooltip : String
    }


map : (a -> b) -> Option a -> Option b
map f old =
    { old | msg = f old.msg }


edit : msg -> Option msg
edit msg =
    Option msg "edit" "Edit"
