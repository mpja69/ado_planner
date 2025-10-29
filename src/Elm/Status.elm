module Status exposing (..)


type Status
    = New
    | Active
    | Resolved
    | Closed


isNew : Status -> Bool
isNew st =
    st == New


isStarted : Status -> Bool
isStarted st =
    st /= New


isClosed : Status -> Bool
isClosed st =
    st == Closed


isOpen : Status -> Bool
isOpen st =
    st /= Closed


isInProgress : Status -> Bool
isInProgress st =
    case st of
        Active ->
            True

        Resolved ->
            True

        _ ->
            False


statusLabel : Status -> String
statusLabel st =
    case st of
        New ->
            "New"

        Active ->
            "Active"

        Resolved ->
            "Resolved"

        Closed ->
            "Closed"


statusFromADO : String -> Status
statusFromADO s =
    case String.toLower s of
        "closed" ->
            Closed

        "resolved" ->
            Resolved

        "active" ->
            Active

        "new" ->
            New

        _ ->
            New
