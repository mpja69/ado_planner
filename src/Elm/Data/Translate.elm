module Data.Translate exposing
    ( PiContext
    , buildPi
    , translate
    )

import Data.Ado as Ado
import Status exposing (statusFromADO)
import Types exposing (..)



-- Context for a chosen PI


type alias PiContext =
    { piRoot : String -- e.g. "Contoso\\ART\\PI-2025-1"
    , sprintNames : List String -- ["Sprint 1", ..., "Sprint 5"]
    , sprintPaths : List String -- piRoot ++ "\\" ++ sprintName for each
    }



-- Helper to build a context from root + sprint names


buildPi : String -> List String -> PiContext
buildPi root sprintNames =
    let
        paths =
            List.map (\n -> root ++ "\\" ++ n) sprintNames
    in
    { piRoot = root, sprintNames = sprintNames, sprintPaths = paths }



-- Resolve a story/feature iterationPath into UI’s StoryIteration


toIteration : PiContext -> String -> Iteration
toIteration ctx path =
    if String.trim path == "" then
        Missing

    else if path == ctx.piRoot then
        WholePI

    else
        case
            List.indexedMap Tuple.pair ctx.sprintPaths
                |> List.filter (\( _, p ) -> p == path)
                |> List.head
        of
            Just ( idx, _ ) ->
                InPI (idx + 1)

            -- 1-based
            Nothing ->
                OutsidePI


testsFromTags : List String -> Tests
testsFromTags tags =
    let
        has t =
            List.any (\x -> String.toUpper x == t) tags
    in
    { sit = has "SIT", uat = has "UAT", e2e = has "E2E" }



-- Main translator: ADO sample → List FeatureRow


translate : PiContext -> Ado.Sample -> List Feature
translate ctx sample =
    let
        storiesByFeature : Int -> List Ado.AdoStory
        storiesByFeature fid =
            List.filter (\s -> s.parentId == fid) sample.stories

        toStory : Ado.AdoStory -> Story
        toStory s =
            { id = s.id
            , title = s.title
            , iteration = toIteration ctx s.iterationPath
            , status = statusFromADO s.state
            }

        toRow : Ado.AdoFeature -> Feature
        toRow f =
            { featureId = f.id
            , title = f.title
            , iteration = toIteration ctx f.iterationPath
            , status = statusFromADO f.state
            , closedDate = Nothing
            , tests = testsFromTags f.tags
            , stories = storiesByFeature f.id |> List.map toStory
            }
    in
    List.map toRow sample.features
