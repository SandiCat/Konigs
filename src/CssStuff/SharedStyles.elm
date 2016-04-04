module CssStuff.SharedStyles (..) where

import Html.CssHelpers exposing (namespace)


type CssClasses
  = Term

type CssIds
  = MouseManipulator

mainNamespace =
  namespace "main"

{ id, class, classList } = mainNamespace