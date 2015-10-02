module Node (Model, view, Context, isMouseWithin, plainNode) where

--import Debug
import Svg
import Svg.Attributes as Att


-- MODEL

type alias Model =
    { pos: (Int, Int)
    , text: String
    , radius: Int
    , thickness: Int
    }

plainNode: (Int, Int) -> Model
plainNode pos =
    Model pos "" 40 7

isMouseWithin: (Int, Int) -> Model -> Bool
isMouseWithin (x, y) model =
    let
        x' = fst model.pos
        y' = snd model.pos
    in
        (x - x')^2 + (y - y')^2 <= model.radius^2


-- VIEW

type alias Context = {}

view: Context -> Model -> Svg.Svg
view context model =
    Svg.g []
        [ Svg.circle
            [ fst model.pos |> toString |> Att.cx
            , snd model.pos |> toString |> Att.cy
            , toString model.radius |> Att.r
            , Att.fill "white"
            , Att.stroke "blue"
            , toString model.thickness |> Att.strokeWidth
            ]
            []
        , Svg.text'
            [ fst model.pos |> toString |> Att.x
            , snd model.pos |> toString |> Att.y
            , Att.textAnchor "middle"
            ]
            [ Svg.text model.text ]
        ]
