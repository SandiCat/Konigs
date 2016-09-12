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
    | TermDisplay
    | ContextMenu
    | MenuIcon
    | Content

type CssIds
    = NothingAtAllToBeSeenHere

css =
    (stylesheet << namespace namespaceName)
    [ (.) TermText
        [ fontFamilies ["Verdana", "Arial", .value sansSerif]
        , px 14 |> fontSize
        ]
    , (.) TermInput
        [ borderColor transparent
        , backgroundColor transparent
        , focus
            [ px 0 |> border
            ]
        ]
    , (.) TermDisplay
        [ displayFlex
        , alignItems center
        , property "justify-content" "center"
        , height (pct 100)
        , width (pct 100)
        , textAlign center
        ]
    , (.) Content
        [ 
        ]
    , each [(.) Node, (.) NodeCont]
        [ position absolute
        ]
    , (.) Node
        [ translate2 (pct -50) (pct -50) |> transform
        ]
    , (.) ContextMenu
        [ displayFlex
        , flexDirection column
        , translate2 (pct -80) (pct -50) |> transform
        , position absolute
        , top (pct 50)
        , padding (px 20)
        ]
    , (.) MenuIcon
        [
        ]
    ]