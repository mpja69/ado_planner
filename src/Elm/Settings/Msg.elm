module Settings.Msg exposing (Msg(..))


type Msg
    = ToggleOpen
    | SetEnableTests Bool
    | SetSitTag String
    | SetUatTag String
    | SetE2eTag String
    | SetEnableTeamTags Bool
    | SetTeamTagsInput String
    | ToggleEditableTests Bool
    | SetLockClosedItems Bool
