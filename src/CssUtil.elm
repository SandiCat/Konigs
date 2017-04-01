module CssUtil exposing (..)

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


layers : Int -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
layers startIndex attrs list =
    List.map2
        (\i elem ->
            Html.div
                [ style [ startIndex + i |> zIndex, Css.position Css.absolute ] ]
                [ elem ]
        )
        (List.range 0 (List.length list))
        list
        |> Html.div attrs


ipx =
    toFloat >> Css.px


position : ( Int, Int ) -> Html.Attribute msg
position ( x, y ) =
    style
        [ ipx x |> Css.left
        , ipx y |> Css.top
        ]


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
