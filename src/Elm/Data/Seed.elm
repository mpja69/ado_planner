module Data.Seed exposing
    ( AreaMini, areas
    , piRoots
    , sprintCountFor, sprintNamesFor
    )

import List.Extra as LE

type alias AreaMini =
    { id : String   -- "Contoso\\ART"
    , name : String -- "ART"
    }

areas : List AreaMini
areas =
    [ { id = "Contoso\\ART", name = "ART" } ]

-- The two PI roots we show as pills (current / next).
piRoots : List String
piRoots  =
    [ "Contoso\\PI 1"
    , "Contoso\\PI 2"
    ]

-- --- NEW: static sprint counts for each PI ---

type alias PiMeta =
    { root : String        -- "Contoso\\PI 1"
    , sprints : Int        -- how many planning sprints (exclude IP)
    }

piTable : List PiMeta
piTable =
    [ { root = "Contoso\\PI 1", sprints = 5 }
    , { root = "Contoso\\PI 2", sprints = 7 }
    ]

sprintCountFor : String -> Maybe Int
sprintCountFor piRoot =
    piTable
        |> List.filter (\p -> p.root == piRoot)
        |> List.head
        |> Maybe.map .sprints

lastSegment : String -> String
lastSegment p =
    p |> String.split "\\" |> List.reverse |> List.head |> Maybe.withDefault p

sprintNamesFor : String -> List String
sprintNamesFor piRoot =
    let
        label =
            lastSegment piRoot

        n =
            sprintCountFor piRoot |> Maybe.withDefault 5
    in
    List.range 1 n
        |> List.map (\i -> label ++ " Sprint " ++ String.fromInt i)
