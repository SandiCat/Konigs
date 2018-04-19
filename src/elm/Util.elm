module Util exposing (..)

import Focus exposing (Focus)
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode
import Math.Vector2 as Vec2 exposing (Vec2)
import Html
import Html.Events
import Keyboard.Extra exposing (Key)


{-| Use instead of `Debug.log` during development to make unexpected erros more obvious.
It has the same API as `Debug.log` but it crashes. Can be easily replaced with `Debug.log`
for production.
-}
crashLog : String -> a -> b
crashLog message value =
    Debug.crash message


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


{-| Sends out a message only if a certian key is pressed
-}
onSpecificKeyPress : Key -> msg -> Html.Attribute msg
onSpecificKeyPress specificKey msg =
    let
        isEnter key =
            if key == specificKey then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        Html.Events.onWithOptions
            "keypress"
            { stopPropagation = False, preventDefault = True }
            (Html.Events.keyCode
                |> Decode.map Keyboard.Extra.fromCode
                |> Decode.andThen isEnter
            )
