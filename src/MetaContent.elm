module MetaContent where

import Svg

import Content.Other as Other
import Content.Term as Term


-- MODEL

type MultiModel
    = MOther Other.Model
    | MTerm Term.Model


-- UPDATE

type MultiAction
    = AOther Other.Action
    | ATerm Term.Action

update: MultiAction -> MultiModel -> Maybe MultiModel
update multiAction multiModel =
    case multiAction of
        AOther action ->
            case multiModel of
                MOther model ->
                    Other.update action model |> MOther |> Just
                otherwise ->
                    Nothing
        ATerm action ->
            case multiModel of
                MTerm model ->
                    Term.update action model |> MTerm |> Just
                otherwise ->
                    Nothing


-- VIEW

view: (Int, Int) -> Int -> MultiModel -> Svg.Svg
view pos radius multiModel =
    case multiModel of
        MOther model ->
            Other.view pos radius model
        MTerm model ->
            Term.view pos radius model