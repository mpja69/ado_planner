module Filters.Msg exposing (Msg(..))

import Filters.AreaSelector as AS
import Filters.Types exposing (TagMode)


type Msg
    = ToggleOpen
    | SetIteration String
    | ToggleTag String
    | SetTagQuery String
    | ClearTagQuery
    | AreaSel AS.Msg
    | SetTeam (Maybe String)
    | ToggleTagsOpen
    | SetTagMode TagMode
    | NoOp
