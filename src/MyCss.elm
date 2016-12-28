module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, html)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Material.Options


namespaceName = "MyCss"

{ id, class, classList } =
    Html.CssHelpers.withNamespace namespaceName

mdlClass: CssClasses -> Material.Options.Property c m
mdlClass =
    toString >> (++) namespaceName >> Material.Options.cs

type CssClasses
    = Node
    | NodeCont
    | TermDisplay
    | ContextMenu
    | MenuIcon
    | Content
    | TermDescription
    | Nodes

type CssIds
    = NothingAtAllToBeSeenHere

css =
    (stylesheet << namespace namespaceName)
    [ (.) TermDisplay
        [ height (pct 100)
        , width (pct 100)
        , textAlign center 
        ]
    , (.) TermDescription
        [ width (px 200)
        , padding (px 20)
        , backgroundColor (hex "8CA8DA")
        , position absolute
        , top (pct 50)
        , left (pct 100)
        , marginLeft (px 20)
        , transform (translateY (pct -50))
        , zIndex 100
        ]
    , (.) Nodes
        [ position absolute
        ]
    , (.) NodeCont
        [ position absolute
        , property "pointer-events" "none"
        ]
    , (.) Node
        [ width (pct 100)
        , height (pct 100)
        , left (pct -50)
        , top (pct -50)
        , position relative
        , property "pointer-events" "auto"
        ]
    , (.) ContextMenu
        [ displayFlex
        , flexDirection column
        , translate2 (pct -80) (pct -50) |> transform
        , position absolute
        , top (pct 50)
        , padding (px 20)
        ]
    , html
        [ overflow hidden
        ]
    ]

zIndex : Int -> Mixin
zIndex i =
    property "z-index" <| toString i