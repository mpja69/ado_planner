module Grid.Logic exposing (..)

import Status
import Types exposing (Feature, FeatureWarn(..), Iteration(..), Story, TestKind(..))


toggleTest : Int -> TestKind -> Feature -> Feature
toggleTest fid kind row =
    if row.featureId /= fid then
        row

    else
        let
            t =
                row.tests

            new =
                case kind of
                    SIT ->
                        { t | sit = not t.sit }

                    UAT ->
                        { t | uat = not t.uat }

                    E2E ->
                        { t | e2e = not t.e2e }
        in
        { row | tests = new }


lastStorySprint : Feature -> Int
lastStorySprint row =
    row.stories
        |> List.filterMap
            (\s ->
                case s.iteration of
                    InPI ix ->
                        Just ix

                    _ ->
                        Nothing
            )
        |> List.maximum
        |> Maybe.withDefault 1


selectWarnKind : Feature -> FeatureWarn
selectWarnKind row =
    if Status.isClosed row.status then
        if hasAnyStoryOpen row then
            WarnStoryOpen

        else
            NoWarn

    else if Status.isNew row.status && hasAnyStoryStarted row then
        WarnStoryStarted

    else
        case featureDeliverySprint row of
            Nothing ->
                NoWarn

            Just _ ->
                if hasStoryAfterDelivery row then
                    WarnStoryAfter

                else
                    NoWarn


isUnscheduled : Iteration -> Bool
isUnscheduled iteration =
    case iteration of
        InPI _ ->
            False

        _ ->
            True


unscheduledStories : Feature -> List Story
unscheduledStories row =
    row.stories
        |> List.filter
            (\s -> isUnscheduled s.iteration)


hasUnscheduledItemsInRow : Feature -> Bool
hasUnscheduledItemsInRow row =
    let
        iterations =
            row.iteration :: List.map .iteration row.stories
    in
    List.any isUnscheduled iterations


hasUnscheduledItems : List Feature -> Bool
hasUnscheduledItems rows =
    List.any hasUnscheduledItemsInRow rows


canInteract : Status.Status -> Bool
canInteract status =
    Status.isOpen status


iterationToSprint : Iteration -> Maybe Int
iterationToSprint itn =
    case itn of
        InPI ix ->
            Just ix

        _ ->
            Nothing


featureDeliverySprint : Feature -> Maybe Int
featureDeliverySprint f =
    iterationToSprint f.iteration


hasStoryAfterDelivery : Feature -> Bool
hasStoryAfterDelivery row =
    case featureDeliverySprint row of
        Nothing ->
            False

        Just d ->
            row.stories
                |> List.any
                    (\s ->
                        case s.iteration of
                            InPI ix ->
                                ix > d

                            _ ->
                                False
                    )


hasAnyStoryStarted : Feature -> Bool
hasAnyStoryStarted row =
    List.any (\s -> Status.isStarted s.status) row.stories


hasAnyStoryOpen : Feature -> Bool
hasAnyStoryOpen row =
    List.any (\s -> Status.isOpen s.status) row.stories
