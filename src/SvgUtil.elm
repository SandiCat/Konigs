module SvgUtil exposing (..)

import Svg
import Svg.Attributes as Att
import Math.Vector2 as Vec2 exposing (Vec2)


circle : number -> String -> String -> Vec2 -> number -> Svg.Svg msg
circle thickness stroke fill pos radius =
    Svg.circle
        [ Vec2.getX pos |> toString |> Att.cx
        , Vec2.getY pos |> toString |> Att.cy
        , toString radius |> Att.r
        , Att.fill fill
        , Att.stroke stroke
        , toString thickness |> Att.strokeWidth
        ]
        []


line : number -> String -> Vec2 -> Vec2 -> Svg.Svg msg
line thickness stroke pos1 pos2 =
    Svg.line
        [ Vec2.getX pos1 |> toString |> Att.x1
        , Vec2.getY pos1 |> toString |> Att.y1
        , Vec2.getX pos2 |> toString |> Att.x2
        , Vec2.getY pos2 |> toString |> Att.y2
        , Att.stroke stroke
        , Att.fill "transparent"
        , toString thickness |> Att.strokeWidth
        ]
        []


position : ( number, number ) -> List (Svg.Attribute msg)
position ( x, y ) =
    [ toString x |> Att.x
    , toString y |> Att.y
    ]


size : number -> number -> List (Svg.Attribute msg)
size width height =
    [ toString width |> Att.width
    , toString height |> Att.height
    ]


translate : number -> number -> Svg.Attribute msg
translate x y =
    "translate("
        ++ toString x
        ++ ","
        ++ toString y
        ++ ")"
        |> Att.transform
