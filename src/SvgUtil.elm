module SvgUtil where

import Svg
import Svg.Attributes as Att


circle: (Int, Int) -> Int -> Int -> String -> String -> Svg.Svg
circle (x, y) radius thickness stroke fill =
    Svg.circle
        [ toString x |> Att.cx
        , toString y |> Att.cy
        , toString radius |> Att.r
        , Att.fill fill
        , Att.stroke stroke
        , toString thickness |> Att.strokeWidth
        ]
        []

line: (Int, Int) -> (Int, Int) -> Int -> String -> Svg.Svg
line (x, y) (x', y') thickness stroke =
    Svg.line
        [ toString x |> Att.x1
        , toString y |> Att.y1
        , toString x' |> Att.x2
        , toString y' |> Att.y2
        , Att.stroke stroke
        , Att.fill "transparent"
        , toString thickness |> Att.strokeWidth
        ]
        []

position: (Int, Int) -> List Svg.Attribute
position (x, y) =
    [ toString x |> Att.x
    , toString y |> Att.y
    ]