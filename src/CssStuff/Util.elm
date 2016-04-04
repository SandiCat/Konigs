module CssStuff.Util where

import Css
import Html.Attributes
import Html


zIndex: Int -> Css.Mixin
zIndex z =
    toString z
    |> Css.property "z-index"

style: List Css.Mixin -> Html.Attribute
style = Css.asPairs >> Html.Attributes.style

layers: Int -> List Html.Attribute -> List Html.Html -> Html.Html
layers startIndex attrs list =
    List.map2
        (\i elem ->
            Html.div
                [ style [ startIndex + i |> zIndex, Css.position Css.absolute ] ]
                [ elem ]
        )
        [0 .. List.length list]
        list
    |> Html.div attrs

-- ipx: Int -> Css.ExplicitLength Css.PxUnits
ipx x =
    toFloat x
    |> Css.px