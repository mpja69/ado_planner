module Data.Filter exposing
    ( AreaMini
      -- , deriveAreaItemsFromSample
      -- , deriveOptionsFromSample
    , derivePiRootsForArea
    , deriveProjectPiRoots
    , deriveSprintNamesFromSample
    , deriveTagsFromSample
    , subsetSample
    )

import Config
import Data.Ado as Ado
import Filters.Types as FT
import Set


type alias AreaMini =
    { id : String
    , name : String
    }



-- deriveAreaItemsFromSample : Ado.Sample -> List AreaMini
-- deriveAreaItemsFromSample s =
--     let
--         roots =
--             s.features
--                 |> List.map .areaPath
--                 |> List.filter (\p -> p /= "")
--                 |> List.map
--                     (\p ->
--                         case String.split "\\" p of
--                             proj :: art :: _ ->
--                                 ( proj ++ "\\" ++ art, art )
--
--                             _ ->
--                                 ( p, p )
--                     )
--                 |> Set.fromList
--                 |> Set.toList
--     in
--     List.map (\( full, short ) -> { id = full, name = short }) roots


deriveProjectPiRoots : Ado.Sample -> List String
deriveProjectPiRoots s =
    let
        paths =
            List.map .iterationPath s.features ++ List.map .iterationPath s.stories

        toPiRoot p =
            case String.split "\\" p of
                proj :: piSeg :: _ ->
                    if String.startsWith "PI " piSeg then
                        Just (proj ++ "\\" ++ piSeg)
                        -- t.ex. "Contoso\PI 1"

                    else
                        Nothing

                _ ->
                    Nothing
    in
    paths
        |> List.filter (\p -> p /= "")
        |> List.filterMap toPiRoot
        |> Set.fromList
        |> Set.toList
        |> List.sort


derivePiRootsForArea : String -> Ado.Sample -> List String
derivePiRootsForArea areaRoot sample =
    let
        -- Project = första segmentet i area-pathen
        project : String
        project =
            case String.split "\\" areaRoot of
                proj :: _ ->
                    proj

                _ ->
                    areaRoot

        -- plocka iteration paths från features + stories
        allIters : List String
        allIters =
            List.map .iterationPath sample.features
                ++ List.map .iterationPath sample.stories

        -- ta ut "project\PI X" (dvs de två första segmenten där andra börjar med "PI ")
        toPiRoot : String -> Maybe String
        toPiRoot p =
            case String.split "\\" p of
                proj :: piSeg :: _ ->
                    if proj == project && (String.startsWith "PI " piSeg || String.startsWith "PI-" piSeg) then
                        Just (proj ++ "\\" ++ piSeg)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    allIters
        |> List.filter (\p -> String.startsWith (project ++ "\\") p)
        |> List.filterMap toPiRoot
        |> Set.fromList
        |> Set.toList
        |> List.sort


subsetSample :
    String -- artAreaPath (full)
    -> String -- piRoot (full)
    -> Ado.Sample
    -> Ado.Sample
subsetSample artAreaPath piRoot sample =
    let
        inArea path =
            path == artAreaPath || String.startsWith (artAreaPath ++ "\\") path

        inPi path =
            path == piRoot || String.startsWith (piRoot ++ "\\") path

        feats =
            List.filter (\f -> inArea f.areaPath && inPi f.iterationPath) sample.features

        featIds =
            feats |> List.map .id |> Set.fromList

        stories =
            List.filter (\s -> Set.member s.parentId featIds) sample.stories
    in
    { features = feats, stories = stories }


{-| Build the tag universe from the fetched subset.

  - Takes TagPolicy so we can hide “test tags” (SIT/UAT/E2E) etc.
  - Returns unique, sorted tags suitable for the chooser.

-}
deriveTagsFromSample : Config.TagPolicy -> Ado.Sample -> List String
deriveTagsFromSample tp sample =
    sample.features
        |> List.concatMap .tags
        |> List.filter (Config.tagVisibleForChooser tp)
        |> Set.fromList
        |> Set.toList
        |> List.sort


normalizeSprintName : String -> String
normalizeSprintName s =
    -- "PI 1 Sprint 2" -> "Sprint 2"
    case List.reverse (String.split " " s) of
        n :: "Sprint" :: _ ->
            "Sprint " ++ n

        _ ->
            s


deriveSprintNamesFromSample : String -> Ado.Sample -> List String
deriveSprintNamesFromSample piRoot sample =
    let
        underPi p =
            String.startsWith (piRoot ++ "\\") p

        lastSeg p =
            p
                |> String.split "\\"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""

        -- collect last segment for every iterationPath under the chosen PI
        segments =
            (List.map .iterationPath sample.features
                ++ List.map .iterationPath sample.stories
            )
                |> List.filter underPi
                |> List.map lastSeg
                |> List.filter (\seg -> seg /= "")

        unique xs =
            xs |> Set.fromList |> Set.toList

        toNum seg =
            seg
                |> String.split " "
                |> List.reverse
                |> List.head
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 999
    in
    segments
        |> unique
        |> List.sortBy toNum
        |> (\names ->
                if List.isEmpty names then
                    -- fallback (dev only)
                    [ "PI 1 Sprint 1", "PI 1 Sprint 2", "PI 1 Sprint 3", "PI 1 Sprint 4" ]

                else
                    names
           )



-- unique : List String -> List String
-- unique xs =
--     xs |> Set.fromList |> Set.toList
-- deriveOptionsFromSample : Ado.Sample -> FT.Options
-- deriveOptionsFromSample s =
--     let
--         -- Extract project\ART (first two segments)
--         areaRoots =
--             s.features
--                 |> List.map .areaPath
--                 |> List.filter (\p -> p /= "")
--                 |> List.map
--                     (\p ->
--                         case String.split "\\" p of
--                             proj :: art :: _ ->
--                                 proj ++ "\\" ++ art
--
--                             _ ->
--                                 p
--                     )
--                 |> Set.fromList
--                 |> Set.toList
--
--         -- Extract project\ART\PI-YYYY-N (first three segments)
--         piRoots =
--             s.features
--                 |> List.map .iterationPath
--                 |> List.filter (\p -> p /= "")
--                 |> List.map
--                     (\p ->
--                         case String.split "\\" p of
--                             proj :: art :: pi :: _ ->
--                                 proj ++ "\\" ++ art ++ "\\" ++ pi
--
--                             _ ->
--                                 p
--                     )
--                 |> Set.fromList
--                 |> Set.toList
--     in
--     { --areas = areaRoots
--       iterations = piRoots
--     }
