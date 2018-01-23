module Util exposing (..)

import Focus exposing (Focus)
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode
import Math.Vector2 as Vec2 exposing (Vec2)


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


decodeVec2 : Decode.Decoder Vec2
decodeVec2 =
    Decode.succeed Vec2.vec2
        |: Decode.field "x" Decode.float
        |: Decode.field "y" Decode.float


encodeVec2 : Vec2 -> Encode.Value
encodeVec2 v =
    Encode.object
        [ ( "x", Vec2.getX v |> Encode.float )
        , ( "y", Vec2.getY v |> Encode.float )
        ]
