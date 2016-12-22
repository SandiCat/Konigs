module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
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