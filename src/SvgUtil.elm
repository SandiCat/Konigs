module SvgUtil exposing (..)

import Svg
import Svg.Attributes as Att


circle : Int -> String -> String -> ( Int, Int ) -> Int -> Svg.Svg msg
circle thickness stroke fill ( x, y ) radius =
    Svg.circle
        [ toString x |> Att.cx
        , toString y |> Att.cy
        , toString radius |> Att.r
        , Att.fill fill
        , Att.stroke stroke
        , toString thickness |> Att.strokeWidth
        ]
        []


line : Int -> String -> ( Int, Int ) -> ( Int, Int ) -> Svg.Svg msg
line thickness stroke ( x, y ) ( x_, y_ ) =
    Svg.line
        [ toString x |> Att.x1
        , toString y |> Att.y1
        , toString x_ |> Att.x2
        , toString y_ |> Att.y2
        , Att.stroke stroke
        , Att.fill "transparent"
        , toString thickness |> Att.strokeWidth
        ]
        []


position : ( Int, Int ) -> List (Svg.Attribute msg)
position ( x, y ) =
    [ toString x |> Att.x
    , toString y |> Att.y
    ]


size : Int -> Int -> List (Svg.Attribute msg)
size width height =
    [ toString width |> Att.width
    , toString height |> Att.height
    ]


translate : Int -> Int -> Svg.Attribute msg
translate x y =
    "translate("
        ++ toString x
        ++ ","
        ++ toString y
        ++ ")"
        |> Att.transform
