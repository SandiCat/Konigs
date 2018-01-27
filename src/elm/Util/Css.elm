module Util.Css exposing (..)

import Css
import Html.Attributes
import Html


zIndex : Int -> Css.Mixin
zIndex z =
    toString z
        |> Css.property "z-index"


style : List Css.Mixin -> Html.Attribute msg
style =
    Css.asPairs >> Html.Attributes.style


userSelect : Bool -> Html.Attribute msg
userSelect selectable =
    let
        value =
            if selectable then
                "text"
            else
                "none"
    in
        Html.Attributes.style
            [ ( "-moz-user-select", value )
            , ( "-webkit-user-select", value )
            , ( "-ms-user-select", value )
            ]
