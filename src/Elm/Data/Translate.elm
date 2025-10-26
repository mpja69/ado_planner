module Data.Translate exposing
    ( PiContext
    , buildPi
    , translate
    )

import Data.Ado as Ado
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


resolveIteration : PiContext -> String -> StoryIteration
resolveIteration ctx path =
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


deliveryFromPath : PiContext -> String -> Maybe Int
deliveryFromPath ctx path =
    case resolveIteration ctx path of
        InPI ix ->
            Just ix

        _ ->
            Nothing



-- Main translator: ADO sample → List FeatureRow


translate : PiContext -> Ado.Sample -> List FeatureRow
translate ctx sample =
    let
        storiesByFeature : Int -> List Ado.AdoStory
        storiesByFeature fid =
            List.filter (\s -> s.parentId == fid) sample.stories

        toStory : Ado.AdoStory -> Story
        toStory s =
            { id = s.id
            , title = s.title
            , iteration = resolveIteration ctx s.iterationPath
            , status = Ado.stateToStatus s.state
            }

        toRow : Ado.AdoFeature -> FeatureRow
        toRow f =
            { featureId = f.id
            , title = f.title
            , delivery = deliveryFromPath ctx f.iterationPath
            , status = Ado.stateToStatus f.state -- ⬅️ NEW
            , tests = testsFromTags f.tags
            , stories = storiesByFeature f.id |> List.map toStory
            }
    in
    List.map toRow sample.features
