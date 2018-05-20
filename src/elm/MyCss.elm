module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, html)
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import Html
import Html.Attributes
import Material.Options
import TypedSvg.Core
import TypedSvg.Attributes


namespaceName =
    "MyCss"


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespaceName


style : List Css.Mixin -> Html.Attribute msg
style =
    Css.asPairs >> Html.Attributes.style


mdlClass : CssClasses -> Material.Options.Property c m
mdlClass =
    toString >> (++) namespaceName >> Material.Options.cs


svgClass : List CssClasses -> TypedSvg.Core.Attribute msg
svgClass =
    List.map (toString >> (++) namespaceName) >> TypedSvg.Attributes.class


type CssClasses
    = Node
    | NodeCont
    | MaxSize
    | HeadingText
    | ContextMenu
    | MenuIcon
    | AbsolutePos
    | Description
    | DescriptionMaximized
    | DescriptionSmall
    | DescriptionContent
    | DescriptionToolbar
    | DescriptionText
    | DescriptionEdit
    | DescriptionEmpty
    | Edge
    | MenuButtons
    | File
    | MentalMap
    | MentalMapSvg
    | Help


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
        , (.) HeadingText
            [ textAlign center
            , width (pct 80)
            , focus [ outline none ]
            ]
        , (.) Description
            [ backgroundColor (rgba 140 168 218 0.8)
            , padding (px 10)
            , position absolute
            , zIndex (int 100)
            , displayFlex
            , flexDirection row
            , userSelect True
            ]
        , (.) DescriptionSmall
            [ height (px 200)
            , top (pct 50)
            , left (pct 100)
            , marginLeft (px 20)
            , transform (translateY (pct -50))
            , children
                [ (.) DescriptionContent
                    [ width (px 300) ]
                ]
            ]
        , (.) DescriptionMaximized
            [ position fixed
            , width (vw 60)
            , height (vh 80)
            , top (vh 10)
            , left (vw 20)
            , children
                [ (.) DescriptionContent
                    [ flexGrow (int 1)
                    , paddingLeft (px 20)
                    ]
                ]
            ]
        , (.) DescriptionEdit
            [ resize none
            , backgroundColor <| rgba 0 0 0 0.1
            , width (pct 100)
            , height (pct 100)
            ]
        , (.) DescriptionToolbar
            [ paddingLeft (px 20)
            , displayFlex
            , flexDirection column
            ]
        , (.) DescriptionText
            [ overflowY auto
            , overflowX hidden
            , height (pct 100)
            ]
        , (.) DescriptionEmpty
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , selector "::selection" [ property "background" "#ee7883" |> important ]
        , (.) AbsolutePos
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
            , pointerEvents None
            ]
        , (.) Node
            [ width (pct 100)
            , height (pct 100)
            , left (pct -50)
            , top (pct -50)
            , position absolute
            , pointerEvents Auto
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
        , (.) Edge
            [ position absolute
            , displayFlex
            , flexDirection row
            , paddingBottom (px 20)
            ]
        , (.) MenuButtons
            [ position absolute
            , bottom (px 20)
            , right (px 20)
            ]
        , (.) File
            [ height (px 50) ]
        , (.) MentalMap
            [ overflow hidden
            , userSelect False
            ]
        , (.) MentalMapSvg
            [ userSelect False
            ]
        , (.) Help
            [ paddingLeft (pct 8)
            , paddingRight (pct 8)
            , margin auto
            , overflow auto
            , height (pct 100)
            ]
        , html
            [ position absolute
            ]
        , selector ".mdl-layout__drawer-button"
            [ color (rgba 140 168 218 0.8)
            , userSelect False
            ]
        , selector ".mdl-layout__drawer.is-visible"
            [ overflow hidden
            ]
        ]


userSelect : Bool -> Css.Mixin
userSelect selectable =
    property "user-select"
        (if selectable then
            "text"
         else
            "none"
        )


pointerEvents : PointerEventsSetting -> Css.Mixin
pointerEvents setting =
    (case setting of
        Auto ->
            "auto"

        None ->
            "none"
    )
        |> property "pointer-events"


type PointerEventsSetting
    = Auto
    | None
