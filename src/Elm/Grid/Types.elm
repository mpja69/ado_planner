module Grid.Types exposing (Model, fromApp)

import Types exposing (Feature)


type alias Model =
    { sprintCount : Int
    , rows : List Feature
    , draggingDelivery : Maybe Int
    , hoverDeliverySprint : Maybe Int
    , draggingStory : Maybe Int
    , hoverStorySprint : Maybe Int
    }



-- thin adapter to build Grid.Model from App.Model (you already have these fields)


fromApp :
    { a
        | sprintCount : Int
        , rows : List Feature
        , draggingDelivery : Maybe Int
        , hoverDeliverySprint : Maybe Int
        , draggingStory : Maybe Int
        , hoverStorySprint : Maybe Int
    }
    -> Model
fromApp a =
    { sprintCount = a.sprintCount
    , rows = a.rows
    , draggingDelivery = a.draggingDelivery
    , hoverDeliverySprint = a.hoverDeliverySprint
    , draggingStory = a.draggingStory
    , hoverStorySprint = a.hoverStorySprint
    }
