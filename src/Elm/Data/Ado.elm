module Data.Ado exposing
    ( AdoFeature
    , AdoStory
    , Sample
    , sample
    )

import Status exposing (Status(..))



-- Raw “ADO-shaped” records (what the REST would give you, simplified)


type alias AdoFeature =
    { id : Int
    , title : String
    , iterationPath : String -- e.g. "Contoso\\ART\\PI-2025-1\\Sprint 3" or "Contoso\\ART\\PI-2025-1"
    , state : String
    , tags : List String -- e.g. [ "SIT", "E2E" ]
    }


type alias AdoStory =
    { id : Int
    , title : String
    , parentId : Int -- feature id
    , iterationPath : String
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
          , iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2"
          , state = "Active"
          , tags = [ "SIT", "E2E" ]
          }
        , { id = 22
          , title = "Feature B"
          , iterationPath = "" --"Contoso\\ART\\PI-2025-1\\Sprint 4"
          , state = "Active"
          , tags = [ "UAT" ]
          }
        , { id = 33
          , title = "Feature C"
          , iterationPath = "Contoso\\ART\\PI-2025-1" -- WholePI
          , state = "New"
          , tags = []
          }
        , { id = 44
          , title = "Feature D"
          , iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 1"
          , state = "Closed"
          , tags = [ "SIT" ]
          }
        , { id = 55
          , title = "Feature E"
          , iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 3"
          , state = "Closed"
          , tags = [ "UAT", "E2E" ]
          }
        , { id = 66
          , title = "Feature F"
          , iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 3"
          , state = "Resolved"
          , tags = [ "UAT", "E2E" ]
          }
        ]
    , stories =
        [ { id = 101, title = "Login API...som har en väldigt lång titel. Den tar liksom aldrig slut. [LONG] ", parentId = 11, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 1", state = "Resolved" }
        , { id = 102, title = "Login UI", parentId = 11, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2", state = "New" }
        , { id = 103, title = "OAuth callback", parentId = 11, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 3", state = "Closed" }
        , { id = 201, title = "Search backend", parentId = 22, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 1", state = "Closed" }
        , { id = 202, title = "Indexing job - som har en väldigt lång titel. Den tar liksom aldrig slut. [LONG] ", parentId = 22, iterationPath = "", state = "New" } -- Missing
        , { id = 203, title = "Search UI", parentId = 22, iterationPath = "Contoso\\ART\\PI-2025-1", state = "Active" } -- WholePI
        , { id = 204, title = "Metrics probe", parentId = 22, iterationPath = "Contoso\\ART\\PI-2024-4\\Sprint 2", state = "New" } -- OutsidePI
        , { id = 301, title = "Report engine", parentId = 33, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2", state = "Active" }
        , { id = 302, title = "Export CSV", parentId = 33, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 4", state = "Resolved" }
        , { id = 401, title = "Export json", parentId = 44, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2", state = "New" }
        , { id = 402, title = "Export markdown", parentId = 44, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 3", state = "Closed" }
        , { id = 501, title = "Export excel", parentId = 55, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2", state = "Closed" }
        , { id = 601, title = "Export text", parentId = 66, iterationPath = "Contoso\\ART\\PI-2025-1\\Sprint 2", state = "New" }
        ]
    }
