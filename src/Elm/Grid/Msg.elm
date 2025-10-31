module Grid.Msg exposing (Msg(..))

import Types exposing (TestKind)


type Msg
    = ToggleTest Int TestKind
    | DeliveryDragStart Int
    | DeliveryDragEnd
    | HoverDelivery (Maybe Int)
    | DeliveryDrop Int
    | StoryDragStart Int
    | StoryDragEnd
    | HoverStory (Maybe Int)
    | StoryDrop Int
    | NoOp
