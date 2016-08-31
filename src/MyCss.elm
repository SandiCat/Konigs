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
    | ContextMenu
    | MenuIcon
    | Content

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
    , (.) Content
        [ position absolute
        , translate2 (pct -50) (pct -50) |> transform
        ]
    , each [(.) Node, (.) NodeCont]
        [ position absolute
        ]
    , (.) ContextMenu
        [ displayFlex
        , flexDirection column
        , left (px -80)
        , position absolute
        , translateY (pct -50) |> transform
        ]
    , (.) MenuIcon
        [
        ]
    ]