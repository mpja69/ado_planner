module Data.Ado exposing
    ( AdoFeature
    , AdoStory
    , Sample
    , filterSample
    , sample
    )

import Set
import Status exposing (Status(..))
import Types exposing (Feature)


type alias Filters =
    { areaPrefix : String
    , piRoot : String
    , includeTags : List String
    }


filterSample : Filters -> { features : List AdoFeature, stories : List AdoStory } -> { features : List AdoFeature, stories : List AdoStory }
filterSample f s =
    let
        tagMatch fs =
            case f.includeTags of
                [] ->
                    True

                tags ->
                    List.any (\t -> List.member t fs) tags

        featureOk af =
            String.startsWith f.areaPrefix af.areaPath
                && String.startsWith f.piRoot af.iterationPath
                && tagMatch af.tags

        keptFeatures =
            List.filter featureOk s.features

        keptFeatureIds =
            List.map .id keptFeatures |> Set.fromList

        storyOk as_ =
            String.startsWith f.areaPrefix as_.areaPath
                && String.startsWith f.piRoot as_.iterationPath
                && Set.member as_.parentId keptFeatureIds

        keptStories =
            List.filter storyOk s.stories
    in
    { features = keptFeatures, stories = keptStories }


type alias AdoFeature =
    { id : Int
    , title : String
    , iterationPath : String -- e.g. "Contoso\\ART\\PI-2025-1\\Sprint 3" or "Contoso\\ART\\PI-2025-1"
    , areaPath : String
    , state : String
    , tags : List String -- e.g. [ "SIT", "E2E" ]
    }


type alias AdoStory =
    { id : Int
    , title : String
    , parentId : Int -- feature id
    , iterationPath : String
    , areaPath : String
    , state : String -- e.g. "New", "Active", "Closed"
    }


type alias Sample =
    { features : List AdoFeature
    , stories : List AdoStory
    }


sample : Sample
sample =
    { features =
        [ { id = 11
          , title = "Feature A som har en väldigt lång titel. Den tar liksom aldrig slut. [LONG] "
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , areaPath = "Contoso\\ART\\Team A"
          , state = "Active"
          , tags = [ "SIT", "E2E", "planned" ]
          }
        , { id = 22
          , title = "Feature B"
          , iterationPath = ""
          , areaPath = "Contoso\\ART\\Team A"
          , state = "Active"
          , tags = [ "UAT" ]
          }
        , { id = 33
          , title = "Feature C"
          , iterationPath = "Contoso\\PI 1"
          , areaPath = "Contoso\\ART\\Team A"
          , state = "New"
          , tags = []
          }
        , { id = 44
          , title = "Feature D"
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , areaPath = "Contoso\\ART\\Team A"
          , state = "Closed"
          , tags = [ "SIT" ]
          }
        , { id = 55
          , title = "Feature E"
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 3"
          , areaPath = "Contoso\\ART\\Team A"
          , state = "Closed"
          , tags = [ "UAT", "E2E" ]
          }
        , { id = 66
          , title = "Feature F"
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 3"
          , areaPath = "Contoso\\ART\\Team A"
          , state = "Resolved"
          , tags = [ "UAT", "E2E", "TeamABC" ]
          }
        ]
    , stories =
        [ { id = 101
          , title = "Login API...som har en väldigt lång titel. Den tar liksom aldrig slut. [LONG] "
          , parentId = 11
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 1"
          , state = "Resolved"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 102
          , title = "Login UI"
          , parentId = 11
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 103
          , title = "OAuth callback"
          , parentId = 11
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 3"
          , state = "Closed"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 201
          , title = "Search backend"
          , parentId = 22
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 1"
          , state = "Closed"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 202
          , title = "Indexing job - som har en väldigt lång titel. Den tar liksom aldrig slut. [LONG] "
          , parentId = 22
          , iterationPath = ""
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }

        -- Missing
        , { id = 203
          , title = "Search UI"
          , parentId = 22
          , iterationPath = "Contoso\\PI 1"
          , state = "Active"
          , areaPath = "Contoso\\ART\\Team A"
          }

        -- WholePI
        , { id = 204
          , title = "Metrics probe"
          , parentId = 22
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }

        -- OutsidePI
        , { id = 301
          , title = "Report engine"
          , parentId = 33
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "Active"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 302
          , title = "Export CSV"
          , parentId = 33
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 4"
          , state = "Resolved"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 401
          , title = "Export json"
          , parentId = 44
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 402
          , title = "Export markdown"
          , parentId = 44
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 3"
          , state = "Closed"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 501
          , title = "Export excel"
          , parentId = 55
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "Closed"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 601
          , title = "Export text"
          , parentId = 66
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }
        , { id = 701
          , title = "Tech improvements"
          , parentId = 0
          , iterationPath = "Contoso\\PI 1\\PI 1 Sprint 2"
          , state = "New"
          , areaPath = "Contoso\\ART\\Team A"
          }
        ]
    }
