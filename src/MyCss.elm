module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)
import Html.CssHelpers


namespaceName = "MyCss"

{ id, class, classList } =
    Html.CssHelpers.withNamespace namespaceName

type CssClasses
    = TermText
    | Node
    | NodeCont
    | TermInput

type CssIds
    = NothingAtAllToBeSeenHere

css =
    (stylesheet << namespace namespaceName)
    [ (.) TermText
        [ textAlign center
        , fontFamilies ["Verdana", "Arial", .value sansSerif]
        , px 14 |> fontSize 
        ]
    , (.) TermInput
        [ borderColor transparent
        , backgroundColor transparent
        , focus
            [ px 0 |> border
            ]
        ]
    , (.) Node
        [ position absolute
        , translate2 (pct -50) (pct -50) |> transform
        ]
    , (.) NodeCont
        [ position absolute
        ]
    ]