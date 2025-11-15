module Filters.Types exposing
    ( Model
    , Options
    , Selection
    , TagMode(..)
    , UIState
    , allTagsL
    , areaL
    , includeTagsL
    , init
    , initWith
    , iterationL
    , iterationsL
    , optionsL
    , readyToFetch
    , selectionL
    , tagModeL
    , tagQueryL
    , tagsEnabled
    , tagsOpenL
    , teamL
    , uiStateL
    )

import Filters.AreaSelector as AS
import Lens exposing (Lens)
import Set exposing (Set)



-- PUBLIC TYPES


type alias Options =
    { --areas : List String -- REMOVED since we have the Filters.AreaSelector.Model
      iterations : List String
    }


type TagMode
    = TagAnd
    | TagOr


type alias Selection =
    { area : Maybe String
    , iteration : Maybe String
    , includeTags : Set String
    , team : Maybe String
    , tagMode : TagMode
    }


type alias UIState =
    { tagsOpen : Bool
    , tagQuery : String
    }


type alias Model =
    { options : Options
    , sel : Selection
    , ui : UIState
    , isOpen : Bool
    , allTags : List String
    , areaSel : AS.Model
    }



-- INIT


init : Model
init =
    { options = { iterations = [] }
    , sel = { area = Nothing, iteration = Nothing, includeTags = Set.empty, team = Nothing, tagMode = TagAnd }
    , isOpen = True
    , allTags = []
    , ui = { tagsOpen = False, tagQuery = "" }
    , areaSel = AS.init { selectedId = Nothing, areas = [], favorites = [] }
    }


initWith : { areas : List AS.AreaMini, iterations : List String, favorites : List String } -> Model
initWith cfg =
    { options = { iterations = cfg.iterations }
    , sel = { area = Nothing, iteration = Nothing, includeTags = Set.empty, team = Nothing, tagMode = TagAnd }
    , isOpen = True
    , allTags = []
    , ui = { tagsOpen = False, tagQuery = "" }
    , areaSel = AS.init { selectedId = Nothing, areas = cfg.areas, favorites = cfg.favorites }
    }



-- HELPERS


tagsEnabled : Model -> Bool
tagsEnabled model =
    not (List.isEmpty model.allTags)


readyToFetch : Selection -> Bool
readyToFetch sel =
    sel.area /= Nothing && sel.iteration /= Nothing



-- Model lenses


selectionL : Lens Model Selection
selectionL =
    { get = .sel
    , set = \v m -> { m | sel = v }
    }


uiStateL : Lens Model UIState
uiStateL =
    { get = .ui
    , set = \v m -> { m | ui = v }
    }


allTagsL : Lens Model (List String)
allTagsL =
    { get = .allTags
    , set = \v m -> { m | allTags = v }
    }


optionsL : Lens Model Options
optionsL =
    { get = .options
    , set = \v m -> { m | options = v }
    }



-- Selection lenses


includeTagsL : Lens Selection (Set String)
includeTagsL =
    { get = .includeTags
    , set = \v s -> { s | includeTags = v }
    }


areaL : Lens Selection (Maybe String)
areaL =
    { get = .area
    , set = \v s -> { s | area = v }
    }


iterationL : Lens Selection (Maybe String)
iterationL =
    { get = .iteration
    , set = \v s -> { s | iteration = v }
    }


teamL : Lens Selection (Maybe String)
teamL =
    { get = .team
    , set = \v s -> { s | team = v }
    }


tagModeL : Lens Selection TagMode
tagModeL =
    { get = .tagMode
    , set = \v s -> { s | tagMode = v }
    }



-- UIState lenses


tagQueryL : Lens UIState String
tagQueryL =
    { get = .tagQuery
    , set = \v u -> { u | tagQuery = v }
    }


tagsOpenL : Lens UIState Bool
tagsOpenL =
    { get = .tagsOpen
    , set = \v u -> { u | tagsOpen = v }
    }



-- Option Lens


iterationsL : Lens Options (List String)
iterationsL =
    { get = .iterations
    , set = \v o -> { o | iterations = v }
    }
