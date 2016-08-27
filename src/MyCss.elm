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

type CssIds
    = NothingAtAllToBeSeenHere

css =
    (stylesheet << namespace namespaceName)
    [ (.) TermText
        [ fontWeight bold
        ]
    , (.) Node
        [ position absolute
        , translate2 (pct -50) (pct -50) |> transform
        ]
    , (.) NodeCont
        [ position absolute
        ]
    ]