module Status exposing (Status(..), fromADO, isClosed, isInProgress, isNew, isOpen, isStarted, label)


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


label : Status -> String
label st =
    case st of
        New ->
            "New"

        Active ->
            "Active"

        Resolved ->
            "Resolved"

        Closed ->
            "Closed"


fromADO : String -> Status
fromADO s =
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



-- Thin, interface-like, adapter, for any item that has a Status field, i.e both the Feature and Story
-- Prefix "Has" indicates that there is a field of ...
-- Suffix "On" indicates that the function is used on a adapter


type alias HasStatus a =
    { a | status : Status }


labelOn : HasStatus a -> String
labelOn s =
    label s.status
