module Option exposing (..)


type alias Option msg =
    { msg : msg
    , icon : String
    }


map : (a -> b) -> Option a -> Option b
map f old =
    Option (f old.msg) old.icon


edit : msg -> Option msg
edit msg =
    Option msg "edit"
