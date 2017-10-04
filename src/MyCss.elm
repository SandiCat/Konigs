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
    | MaxSize
    | TermDisplay
    | TermText
    | ContextMenu
    | MenuIcon
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
        [ (.) MaxSize
            [ height (pct 100)
            , width (pct 100)
            ]
        , (.) TermDisplay
            [ height (pct 100)
            , width (pct 100)
            ]
        , (.) TermText
            [ textAlign center
            , width (pct 80)
            , focus [ outline none ]
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
            {- Two things. First, this exists so that Node could properly center against
               the width and height of it's parent. Node and NodeCont cannot be merged.
               Second, pointer-events is here so that the NodeCont div doesn't interfere
               with background clicks or node clicks, since it's a box with width and height
               position on top of the node.
            -}
            [ position absolute
            , property "pointer-events" "none"
            ]
        , (.) Node
            [ width (pct 100)
            , height (pct 100)
            , left (pct -50)
            , top (pct -50)
            , position absolute
            , property "pointer-events" "auto"
            , children
                [ Css.Elements.div
                    [ width (pct 100)
                    , height (pct 100)
                    , position absolute
                    ]
                ]
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
