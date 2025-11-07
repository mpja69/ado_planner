module Filters.Msg exposing (Msg(..))

import Filters.AreaSelector as AS


type Msg
    = ToggleOpen
    | SetIteration String
    | ToggleTag String
    | SetTagQuery String
    | ClearTagQuery
    | AreaSel AS.Msg
    | NoOp
