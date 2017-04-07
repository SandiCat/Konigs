#!/usr/bin/python

from os import listdir
from os.path import isfile, join

path = join("src", "Content")
contents = [f[:-4] for f in listdir(path) if isfile(join(path, f))]


def generate_part(template):
    return "\n".join([template.format(content) for content in contents])

def generate_part_union(template, indent):
    return "\n".join([indent + ("= " if i == 0 else "| ") + template.format(content) for i, content in enumerate(contents)])

parts = [
    generate_part("import Content.{0} as {0}"),
    generate_part_union("Mdl{0} {0}.Model", "    "),
    generate_part("""\
        Mdl{0} model ->
            {0}.menuOptions
            |> List.map (Option.map Msg{0})"""),
    generate_part_union("Msg{0} {0}.Msg", "    "),
    generate_part("""\
        Msg{0} action ->
            case multiModel of
                Mdl{0} model ->
                    let
                        (model_, cmd) = {0}.update action model
                    in
                        Just (Mdl{0} model_, Cmd.map Msg{0} cmd)
                _ ->
                    Debug.log mismatchError Nothing"""),
    generate_part("""\
        Mdl{0} model ->
            {0}.viewInside pos radius model
            |> Html.map Msg{0}"""),
    generate_part("""\
        Mdl{0} model ->
            {0}.viewOutside pos radius model
            |> Html.map Msg{0}"""),
    generate_part("""\
        Mdl{0} model ->
            {0}.subscriptions model
                |> Sub.map Msg{0}""")
]

base_code = """\
-- Autogenerated by generate_metacontent.py
-- If only Elm had type classes...

module MetaContent exposing (..) 

import Html
import MyCss
import Option exposing (Option)

{}


-- MODEL

type MultiModel
{}

menuOptions: MultiModel -> List (Option MultiMsg)
menuOptions multiModel =
    case multiModel of
{}


-- UPDATE

type MultiMsg
{}

mismatchError: String
mismatchError = "MetaContent.update msg model type mismatch"

update: MultiMsg -> MultiModel -> Maybe (MultiModel, Cmd MultiMsg)
update multiMsg multiModel =
    case multiMsg of
{}


-- VIEW

viewInside: MultiModel -> Html.Html MultiMsg
viewInside multiModel =
    case multiModel of
{}

viewOutside: MultiModel -> Html.Html MultiMsg
viewOutside multiModel =
    case multiModel of
{}


-- SUBSCRIPTIONS

subscriptions : MultiModel -> Sub MultiMsg
subscriptions multiModel =
    case multiModel of
{}
"""

f = open(join("src", "MetaContent.elm"), "w")
f.write(base_code.format(*parts))

print("Successfully generated MetaContent.elm!")