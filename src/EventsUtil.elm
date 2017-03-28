module EventsUtil exposing (..)

import Html.Events as Events
import Html
import Json.Decode
import Material.Options as Options


noPropagation : String -> msg -> Html.Attribute msg
noPropagation eventName msg =
    Events.onWithOptions
        eventName
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed msg)


noPropagationMdl : String -> msg -> Options.Property c msg
noPropagationMdl eventName msg =
    Options.onWithOptions
        eventName
        { stopPropagation = True, preventDefault = False }
        (Json.Decode.succeed msg)


onMouseDownNoProp =
    noPropagation "mousedown"


onMouseUpNoProp =
    noPropagation "mouseup"


doubleClickMdlNoProp =
    noPropagationMdl "dblclick"


onClickMdlNoProp =
    noPropagationMdl "click"


onMouseDownMdlNoProp =
    noPropagationMdl "mousedown"


onMouseOverMdlNoProp =
    noPropagationMdl "mouseover"
