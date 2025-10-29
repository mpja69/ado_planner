module Types exposing (..)

import Status exposing (Status)
import Time exposing (Posix)



-- UI/Domain types used by the app


type Iteration
    = InPI Int -- sprint index 1..N within the chosen PI
    | Missing -- no iteration assigned
    | WholePI -- iteration path is the PI root (no sprint)
    | OutsidePI -- iteration points to another PI / outside chosen range


type alias Story =
    { id : Int
    , title : String
    , iteration : Iteration
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


type FeatureWarn
    = NoWarn
    | WarnAfter -- A feature has stories AFTER it self, (with regards to IterationPath)
    | WarnStoriesNotDone -- Some of the feature's stories are not done, (while the feture is)
    | WarnFeatureLagging -- Some of the feature's stories have started, but the feature has not


type alias Feature =
    { featureId : Int
    , title : String
    , iteration : Iteration --Maybe Int -- 1..N if feature iteration is a sprint; Nothing for WholePI/Missing/Outside
    , status : Status
    , closedDate : Maybe Posix -- NEW: ADO ClosedDate (if available)
    , tests : Tests -- mapped from tags later
    , stories : List Story
    }


type AdoCmd
    = SetStoryIteration { storyId : Int, toSprintIx : Int }
    | SetFeatureIteration { featureId : Int, toSprintIx : Int }
    | SetFeatureTags { featureId : Int, sit : Bool, uat : Bool, e2e : Bool }
