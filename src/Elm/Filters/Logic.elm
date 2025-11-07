module Filters.Logic exposing
    ( deriveOptionsFromADO
    , deriveTagsFromADO
    )

import Data.Ado as Ado
import List.Extra as LE


{-| Build the Area and Iteration option lists from ADO sample.

Areas (ARTs): take the **2nd segment** of iteration-like paths, e.g.
"Contoso\\ART\\PI-2025-1\\Sprint 2" -> "ART"

PIs: any **segment** that starts with "PI-" or "PI ".

-}
deriveOptionsFromADO :
    Ado.Sample
    -> { areas : List String, iterations : List String }
deriveOptionsFromADO sample =
    let
        paths : List String
        paths =
            -- collect paths from both features and stories
            List.map .iterationPath sample.features
                ++ List.map .iterationPath sample.stories

        splitPath : String -> List String
        splitPath p =
            String.split "\\" p
                |> List.filter (\s -> s /= "")

        segments : List (List String)
        segments =
            List.map splitPath paths

        areas0 : List String
        areas0 =
            segments
                |> List.filter (\segs -> List.length segs >= 2)
                |> List.map (\segs -> List.drop 1 segs |> List.head)
                |> List.filterMap identity

        isPiSeg : String -> Bool
        isPiSeg seg =
            String.startsWith "PI-" seg || String.startsWith "PI " seg

        pis0 : List String
        pis0 =
            segments
                |> List.concat
                |> List.filter isPiSeg
    in
    { areas = areas0 |> LE.unique |> List.sort
    , iterations = pis0 |> LE.unique |> List.sort
    }


{-| Given the current selection, derive the **union of feature tags**
that match the selected Area (ART) and PI (iteration).

Note: since `Ado.Feature` has `.tags`, we filter features:

  - area filter: second segment of feature.iterationPath equals selected area
  - iteration filter: any segment equals selected PI (e.g. "PI-2025-1")

-}
deriveTagsFromADO :
    { area : Maybe String, iteration : Maybe String }
    -> Ado.Sample
    -> List String
deriveTagsFromADO sel sample =
    let
        splitPath : String -> List String
        splitPath p =
            String.split "\\" p |> List.filter (\s -> s /= "")

        -- Build "project\ART" from a feature's areaPath
        areaRootOf : String -> String
        areaRootOf areaPath =
            case splitPath areaPath of
                proj :: art :: _ ->
                    proj ++ "\\" ++ art

                _ ->
                    areaPath

        -- Build "project\ART\PI-YYYY-N" from a feature's iterationPath
        piRootOf : String -> String
        piRootOf iterPath =
            case splitPath iterPath of
                proj :: art :: pi :: _ ->
                    proj ++ "\\" ++ art ++ "\\" ++ pi

                _ ->
                    iterPath

        featureMatches : Ado.AdoFeature -> Bool
        featureMatches f =
            let
                fAreaRoot =
                    areaRootOf f.areaPath

                fPiRoot =
                    piRootOf f.iterationPath

                areaOk =
                    case sel.area of
                        Nothing ->
                            True

                        Just selectedAreaRoot ->
                            fAreaRoot == selectedAreaRoot

                piOk =
                    case sel.iteration of
                        Nothing ->
                            True

                        Just selectedPiRoot ->
                            fPiRoot == selectedPiRoot
            in
            areaOk && piOk
    in
    sample.features
        |> List.filter featureMatches
        |> List.concatMap .tags
        |> LE.unique
        |> List.sort
