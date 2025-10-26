module Types exposing (..)

import Time exposing (Posix)



-- UI/Domain types used by the app


type Status
    = Todo
    | Doing
    | Done


type StoryIteration
    = InPI Int -- sprint index 1..N within the chosen PI
    | Missing -- no iteration assigned
    | WholePI -- iteration path is the PI root (no sprint)
    | OutsidePI -- iteration points to another PI / outside chosen range


type alias Story =
    { id : Int
    , title : String
    , iteration : StoryIteration
    , status : Status
    }


type alias Tests =
    { sit : Bool
    , uat : Bool
    , e2e : Bool
    }


type TestKind
    = SIT
    | UAT
    | E2E



-- ROW MODE (derived from feature status + unlocks)


type RowMode
    = Active
    | DoneCompact
    | DoneExpanded


type alias FeatureRow =
    { featureId : Int
    , title : String
    , delivery : Maybe Int -- 1..N if feature iteration is a sprint; Nothing for WholePI/Missing/Outside
    , status : Status
    , closedDate : Maybe Posix -- NEW: ADO ClosedDate (if available)
    , tests : Tests -- mapped from tags later
    , stories : List Story
    }


type AdoCmd
    = SetStoryIteration { storyId : Int, toSprintIx : Int }
    | SetFeatureIteration { featureId : Int, toSprintIx : Int }
    | SetFeatureTags { featureId : Int, sit : Bool, uat : Bool, e2e : Bool }


type FeatureWarn
    = NoWarn
    | WarnAfter
    | WarnNeedsDelivery
