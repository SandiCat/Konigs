module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, html)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Material.Options


namespaceName =
    "MyCss"


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespaceName


mdlClass : CssClasses -> Material.Options.Property c m
mdlClass =
    toString >> (++) namespaceName >> Material.Options.cs


type CssClasses
    = Node
    | NodeCont
    | Term
    | TermDisplay
    | TermInput
    | ContextMenu
    | MenuIcon
    | Content
    | TermDescription
    | Nodes
    | DescriptionToolbar
    | DescriptionText
    | DescriptionEmpty


type CssIds
    = NothingAtAllToBeSeenHere


(.) =
    -- to avoid a name conflict
    Css.class


css =
    (stylesheet << namespace namespaceName)
        [ (.) Term
            [ height (pct 100)
            , width (pct 100)
            ]
        , (.) TermDisplay
            [ textAlign center
            , height (pct 100)
            ]
        , (.) TermInput
            [ displayFlex
            , flexDirection row
            , alignItems center
            , width (px 200)
            , backgroundColor (rgba 140 168 218 0.8)
            , paddingLeft (px 10)
            , paddingRight (px 10)
            , height (px 50)
            , marginTop (px 20)
            , position absolute
            , transform (translateX (pct -50))
            , left (pct 50)
            ]
        , (.) TermDescription
            [ minHeight (px 100)
            , padding (px 10)
            , backgroundColor (rgba 140 168 218 0.8)
            , position absolute
            , top (pct 50)
            , left (pct 100)
            , marginLeft (px 20)
            , transform (translateY (pct -50))
            , zIndex 100
            , displayFlex
            , flexDirection row
            ]
        , (.) DescriptionToolbar
            [ paddingLeft (px 20)
            , float right
            ]
        , (.) DescriptionText
            [ width (px 200) ]
        , (.) DescriptionEmpty
            [ width (px 200)
            , height (pct 100)
            , verticalAlign middle
            , textAlign center
            ]
        , selector "::selection" [ property "background" "#ee7883" |> important ]
        , selector "::-moz-selection" [ property "background" "#ee7883" |> important ]
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
            , position absolute
            ]
        ]


zIndex : Int -> Mixin
zIndex i =
    property "z-index" <| toString i
