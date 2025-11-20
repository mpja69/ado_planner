module Grid.Types exposing (Model, empty)

import Set exposing (Set)
import Types exposing (Feature)


type alias Model =
    { sprintCount : Int
    , rows : List Feature
    , draggingDelivery : Maybe Int
    , hoverDeliverySprint : Maybe Int
    , draggingStory : Maybe Int
    , hoverStorySprint : Maybe Int
    , showUnscheduled : Bool
    , hiddenSprints : Set Int
    }


empty : Model
empty =
    { sprintCount = 0
    , rows = []
    , draggingDelivery = Nothing
    , hoverDeliverySprint = Nothing
    , draggingStory = Nothing
    , hoverStorySprint = Nothing
    , showUnscheduled = True
    , hiddenSprints = Set.empty
    }
