module Settings.Types exposing
    ( Model
    , fromConfig
    , toConfig
    )

import Config
import Set exposing (Set)


type alias Model =
    { isOpen : Bool
    , enableTests : Bool
    , sitTag : String
    , uatTag : String
    , e2eTag : String
    , enableTeamTags : Bool
    , teamTagsInput : String
    , editableTests : Bool
    }


fromConfig : Config.Config -> Model
fromConfig cfg =
    let
        tp =
            cfg.tags

        teamTagsStr =
            tp.teamTags
                |> Set.toList
                |> String.join ", "
    in
    { isOpen = False
    , enableTests = tp.enableTests
    , sitTag = tp.testTags.sit
    , uatTag = tp.testTags.uat
    , e2eTag = tp.testTags.e2e
    , enableTeamTags = tp.enableTeamTags
    , teamTagsInput = teamTagsStr
    , editableTests = tp.editableTests
    }


toConfig : Model -> Config.Config -> Config.Config
toConfig m oldCfg =
    let
        parseTeamTags : String -> Set String
        parseTeamTags raw =
            raw
                |> String.split ","
                |> List.map String.trim
                |> List.filter (\s -> s /= "")
                |> Set.fromList

        newTagPolicy =
            { enableTests = m.enableTests
            , testTags =
                { sit = m.sitTag
                , uat = m.uatTag
                , e2e = m.e2eTag
                }
            , enableTeamTags = m.enableTeamTags
            , teamTags = parseTeamTags m.teamTagsInput
            , editableTests = m.editableTests
            }
    in
    { oldCfg | tags = newTagPolicy }
