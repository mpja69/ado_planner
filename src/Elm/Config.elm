module Config exposing
    ( AreaMode(..)
    , Config
    , TagPolicy
    , default
    , isTeamTag
      -- , isTestTag
    , tagVisibleForChooser
    )

import Set exposing (Set)


type AreaMode
    = ArtAreaBasedOnTeam -- Välj team i UI; fetch features UNDER teamets parent-area (ART-nivå)
    | TeamAreaIsFeatureArea -- Teamets area ÄR också feature-arean


type alias TagPolicy =
    { enableTests : Bool
    , testTags :
        { sit : String
        , uat : String
        , e2e : String
        }
    , enableTeamTags : Bool
    , teamTags : Set String
    }


type alias Config =
    { areaMode : AreaMode
    , tags : TagPolicy
    }


default : Config
default =
    { areaMode = ArtAreaBasedOnTeam
    , tags =
        { enableTests = True
        , testTags = { sit = "SIT", uat = "UAT", e2e = "E2E" }
        , enableTeamTags = False
        , teamTags = Set.empty
        }
    }



-- Helpers ----------------------------
-- isTestTag : TagPolicy -> String -> Bool
-- isTestTag tp t =
--     tp.enableTests && Set.member t tp.testTags


isTeamTag : TagPolicy -> String -> Bool
isTeamTag tp t =
    tp.enableTeamTags && Set.member t tp.teamTags



-- Används när vi bygger listan av valbara taggar i filterpanelen
-- wherever you filter visible tags


tagVisibleForChooser : TagPolicy -> String -> Bool
tagVisibleForChooser tp tag =
    let
        up =
            String.toUpper

        isTestTag =
            List.member (up tag)
                [ up tp.testTags.sit, up tp.testTags.uat, up tp.testTags.e2e ]
    in
    not (tp.enableTests && isTestTag)
