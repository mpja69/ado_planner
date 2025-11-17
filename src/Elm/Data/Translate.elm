module Data.Translate exposing
    ( PiContext
    , buildPi
    , emptyPi
    , iterationPathForSprintIx
    , translate
    )

import Config
import Data.Ado as Ado
import Status
import Types exposing (..)



-- Context for a chosen PI


type alias PiContext =
    { piRoot : String
    , sprintNames : List String
    , sprintPaths : List String
    }


emptyPi : PiContext
emptyPi =
    { piRoot = ""
    , sprintNames = []
    , sprintPaths = []
    }



-- Helper to build a context from root + sprint names


buildPi : String -> List String -> PiContext
buildPi root sprintNames =
    let
        paths =
            List.map (\n -> root ++ "\\" ++ n) sprintNames
    in
    { piRoot = root, sprintNames = sprintNames, sprintPaths = paths }


iterationPathForSprintIx : Int -> PiContext -> Maybe String
iterationPathForSprintIx ix ctx =
    -- InPI ix använder 1-baserad indexering (Sprint 1 = ix 1)
    if ix <= 0 then
        Nothing

    else
        ctx.sprintPaths
            |> List.drop (ix - 1)
            |> List.head



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


deriveTests : Config.TagPolicy -> List String -> Tests
deriveTests tp tags =
    let
        up s =
            String.toUpper s

        tagsU =
            List.map up tags

        has t =
            List.member (up t) tagsU
    in
    { sit = tp.enableTests && has tp.testTags.sit
    , uat = tp.enableTests && has tp.testTags.uat
    , e2e = tp.enableTests && has tp.testTags.e2e
    }



-- Main translator: ADO sample → List FeatureRow


translate : Config.Config -> PiContext -> Ado.Sample -> List Feature
translate cfg ctx sample =
    let
        storiesByFeature : Int -> List Ado.AdoStory
        storiesByFeature fid =
            List.filter (\s -> s.parentId == fid) sample.stories

        toStory : Ado.AdoStory -> Story
        toStory s =
            { id = s.id
            , title = s.title
            , iteration = toIteration ctx s.iterationPath
            , status = Status.fromADO s.state
            }

        toRow : Ado.AdoFeature -> Feature
        toRow f =
            { featureId = f.id
            , title = f.title
            , iteration = toIteration ctx f.iterationPath
            , status = Status.fromADO f.state
            , closedDate = Nothing
            , tests = deriveTests cfg.tags f.tags -- THIS NEW INSTEAD OF THE ONE BELOW
            , stories = storiesByFeature f.id |> List.map toStory
            , tags = f.tags -- List.filter (Config.tagVisibleForChooser cfg.tags) f.tags
            }
    in
    List.map toRow sample.features
