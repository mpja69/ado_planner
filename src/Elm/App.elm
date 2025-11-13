port module App exposing (main)

import Browser
import Components.Rails exposing (..)
import Config
import Data.Ado as Ado
import Data.Filter as F
import Data.Seed as Seed exposing (piRoots)
import Data.Translate as T
import Dict exposing (Dict)
import Filters.AreaSelector as AS
import Filters.Msg as FM
import Filters.Types as FT
import Filters.Update as FU
import Filters.View as FV
import Grid.Msg as GM
import Grid.Types as GT
import Grid.Update as GU
import Grid.View as GV
import Html exposing (Html, div, text)
import Html.Attributes as A
import Lens
import Set
import Status exposing (..)
import Types exposing (..)
import Ui exposing (UiSize(..))



-- PORTS


port requestIterations : () -> Cmd msg


port receiveIterations : (List String -> msg) -> Sub msg


port requestAreas : () -> Cmd msg


port receiveAreas : (List { id : String, name : String } -> msg) -> Sub msg


port receivePiMeta : (List { root : String, sprintCount : Int } -> msg) -> Sub msg


port requestData : { area : String, pi : String } -> Cmd msg


port receiveData :
    ({ features : List Ado.AdoFeature
     , stories : List Ado.AdoStory
     }
     -> msg
    )
    -> Sub msg



-- TODO:
-- Add planning of of stories without Feature
-- Maybe have specific rows for different Type of Work
--    Technical Improvements
--    Test Automation
-- Add a Settings where you can
--    Map tests -> Tags
--    Map ToW -> Tags
-- Make use of Feature Flags


type alias Model =
    { sprintCount : Int
    , rows : List Feature
    , draggingDelivery : Maybe Int
    , hoverDeliverySprint : Maybe Int
    , draggingStory : Maybe Int
    , hoverStorySprint : Maybe Int
    , pi : T.PiContext
    , outbox : List AdoCmd
    , filters : FT.Model
    , config : Config.Config
    , piSprintCount : Dict String Int
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        -- Temporary sprint labels so the grid has headers before any fetch.
        -- These will be replaced when a real PI is chosen and runIntents builds a new context.
        sprintNames =
            [ "Sprint 1", "Sprint 2", "Sprint 3", "Sprint 4", "Sprint 5" ]

        -- Harmless placeholder PI context; replaced after a real selection.
        ctx =
            T.buildPi "Contoso\\PI 1" sprintNames

        -- Filters: start from FT.init, plug in hard-coded PI roots, and seed the custom Area selector
        base : FT.Model
        base =
            FT.init

        filters0 : FT.Model
        filters0 =
            { base | options = { iterations = Seed.piRoots } }

        -- Feed Areas + (optional) favorites into the custom selector
        ( as1, _ ) =
            AS.update (AS.ReplaceAreas Seed.areas) filters0.areaSel

        ( as2, _ ) =
            AS.update (AS.ReplaceFavorites []) as1

        filters1 : FT.Model
        filters1 =
            { filters0 | areaSel = as2 }

        model =
            { sprintCount = List.length sprintNames
            , rows = []
            , draggingDelivery = Nothing
            , hoverDeliverySprint = Nothing
            , draggingStory = Nothing
            , hoverStorySprint = Nothing
            , pi = ctx
            , outbox = []
            , filters = filters1
            , config = Config.default
            , piSprintCount = Dict.empty
            }
    in
    ( model, Cmd.batch [ requestIterations (), requestAreas () ] )



-- Kör alla AdoCmd-intents synkront och töm outbox


runIntents : Model -> Model
runIntents model =
    let
        step : AdoCmd -> Model -> Model
        step intent m =
            case intent of
                FetchFeatures { artAreaPath, piRoot } ->
                    let
                        sub =
                            F.subsetSample artAreaPath piRoot Ado.sample

                        -- prefer count from piSprintCount; fallback to your previous derive
                        sCount =
                            Dict.get piRoot model.piSprintCount
                                |> Maybe.withDefault 5

                        sprintNames =
                            List.map (\n -> "Sprint " ++ String.fromInt n) (List.range 1 sCount)

                        ctx =
                            T.buildPi piRoot sprintNames

                        -- NEW: derive visible tag list from the subset
                        allTags =
                            F.deriveTagsFromSample model.config.tags sub

                        -- Keep only selected tags that still exist
                        allowed =
                            Set.fromList allTags

                        keptSel =
                            Set.intersect model.filters.sel.includeTags allowed

                        filters2 =
                            model.filters
                                |> Lens.set FT.allTagsL allTags
                                |> Lens.set (Lens.compose FT.selectionL FT.includeTagsL) keptSel
                    in
                    { m
                        | rows = T.translate model.config ctx sub
                        , sprintCount = sCount
                        , outbox = []
                        , filters = filters2
                    }

                _ ->
                    -- not handled here; leave model as-is
                    m
    in
    List.foldl step model model.outbox



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ --receiveIterations GotIterations
          receiveAreas GotAreas
        , receivePiMeta GotPiMeta
        , receiveData GotData
        ]



-- MESSAGES


type Msg
    = Grid GM.Msg
    | Filters FM.Msg
      -- | GotIterations (List String)
    | GotAreas (List { id : String, name : String })
    | GotPiMeta (List { root : String, sprintCount : Int })
    | GotData { features : List Ado.AdoFeature, stories : List Ado.AdoStory }
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotAreas miniAreas ->
            let
                -- kör igenom AreaSelector.update med ReplaceAreas
                ( as2, chosen ) =
                    AS.update (AS.ReplaceAreas (List.map (\r -> { id = r.id, name = r.name }) miniAreas)) model.filters.areaSel

                keepSel =
                    case ( model.filters.sel.area, chosen ) of
                        -- AreaSelector själv returnerar chosen = Just id vid SelectArea.
                        -- Här kommer ReplaceAreas, så chosen = Nothing. Vi bevarar befintligt val om det finns i listan.
                        ( Just old, _ ) ->
                            if List.any (\a -> a.id == old) miniAreas then
                                Just old

                            else
                                Nothing

                        ( Nothing, _ ) ->
                            Nothing

                filters1 =
                    model.filters

                sel1 =
                    filters1.sel

                filters2 =
                    { filters1
                        | areaSel = as2
                        , sel = { sel1 | area = keepSel }
                    }
            in
            ( { model | filters = filters2 }, Cmd.none )

        -- GotIterations piRoots ->
        --     -- Just store the two PI roots in the filter options.
        --     -- (No auto-select; the user still clicks the 2-pill picker.)
        --     let
        --         opts1 =
        --             model.filters.options
        --
        --         opts2 =
        --             { opts1 | iterations = piRoots }
        --
        --         filters1 =
        --             model.filters
        --
        --         filters2 =
        --             { filters1 | options = opts2 }
        --     in
        --     ( { model | filters = filters2 }, Cmd.none )
        Grid gm ->
            let
                -- convert App.Model -> Grid.Model, then back by copying fields
                ( g2, intents ) =
                    GU.update gm (GT.fromApp model)

                model2 =
                    { model
                        | sprintCount = g2.sprintCount
                        , rows = g2.rows
                        , draggingDelivery = g2.draggingDelivery
                        , hoverDeliverySprint = g2.hoverDeliverySprint
                        , draggingStory = g2.draggingStory
                        , hoverStorySprint = g2.hoverStorySprint
                        , outbox = intents ++ model.outbox
                    }
            in
            ( model2, Cmd.none )

        Filters fmsg ->
            let
                filters2 =
                    FU.update fmsg model.filters

                cmdFetch =
                    case ( filters2.sel.area, filters2.sel.iteration ) of
                        ( Just art, Just pi ) ->
                            if List.isEmpty model.rows then
                                -- SEND PORT instead of building local sample intent
                                requestData { area = art, pi = pi }

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | filters = filters2 }, cmdFetch )

        -- Filters fmsg ->
        --     let
        --         filters2 =
        --             FU.update fmsg model.filters
        --
        --         newIntents =
        --             case ( filters2.sel.area, filters2.sel.iteration ) of
        --                 ( Just art, Just pi ) ->
        --                     if List.isEmpty model.rows then
        --                         [ FetchFeatures { artAreaPath = art, piRoot = pi } ]
        --
        --                     else
        --                         []
        --
        --                 _ ->
        --                     []
        --
        --         model2 =
        --             { model
        --                 | filters = filters2
        --                 , outbox = model.outbox ++ newIntents
        --             }
        --                 |> runIntents
        --     in
        --     ( model2, Cmd.none )
        GotPiMeta rows ->
            let
                -- build Dict root -> sprintCount
                dict =
                    List.foldl
                        (\r acc -> Dict.insert r.root r.sprintCount acc)
                        Dict.empty
                        rows

                -- the two PI roots we’ll show as pills
                roots : List String
                roots =
                    List.map .root rows

                -- update Filters: options.iterations = roots
                filters1 =
                    model.filters

                opts1 =
                    filters1.options

                sel1 =
                    filters1.sel

                keepIter =
                    case sel1.iteration of
                        Just old ->
                            if List.member old roots then
                                Just old

                            else
                                Nothing

                        Nothing ->
                            Nothing

                opts2 =
                    { opts1 | iterations = roots }

                filters2 =
                    { filters1
                        | options = opts2
                        , sel = { sel1 | iteration = keepIter }
                    }
            in
            ( { model | piSprintCount = dict, filters = filters2 }, Cmd.none )

        GotData payload ->
            let
                -- Data is already filtered by (area, pi) in TS, so we can translate directly
                sample =
                    { features = payload.features
                    , stories = payload.stories
                    }

                -- pick sprint count for the currently selected PI (fallback to 5)
                piRoot =
                    Maybe.withDefault "" model.filters.sel.iteration

                sCount =
                    Dict.get piRoot model.piSprintCount |> Maybe.withDefault 5

                piSeg : String
                piSeg =
                    -- last segment of "Alfa Laval Portfolio\PI 30" -> "PI 30"
                    case List.reverse (String.split "\\" piRoot) of
                        seg :: _ ->
                            seg

                        [] ->
                            "PI"

                sprintNames : List String
                sprintNames =
                    List.map
                        (\n -> piSeg ++ " Sprint " ++ String.fromInt n)
                        (List.range 1 sCount)

                ctx =
                    T.buildPi piRoot sprintNames

                -- DEBUG: leave these while verifying
                _ =
                    Debug.log "GotData counts (features,stories)"
                        ( List.length payload.features, List.length payload.stories )

                _ =
                    case List.filter (\s -> s.parentId /= 0) payload.stories |> List.head of
                        Just sNonZero ->
                            Debug.log "First story with parentId /= 0 (title,id,parent)"
                                ( sNonZero.title, sNonZero.id, sNonZero.parentId )

                        Nothing ->
                            Debug.log "No stories with parentId /= 0" ( "", 0, 0 )

                _ =
                    case payload.stories |> List.head of
                        Just s ->
                            Debug.log "First story (any) (title,id,parent)"
                                ( s.title, s.id, s.parentId )

                        Nothing ->
                            Debug.log "First story (none)" ( "", 0, 0 )

                rows2 =
                    T.translate model.config ctx sample

                -- refresh tag list from the received data
                allTags =
                    F.deriveTagsFromSample model.config.tags sample

                allowed =
                    Set.fromList allTags

                -- EITHER keep only still-existing tags...
                keptSel =
                    Set.intersect model.filters.sel.includeTags allowed

                filters2 =
                    model.filters
                        |> Lens.set FT.allTagsL allTags
                        |> Lens.set (Lens.compose FT.selectionL FT.includeTagsL) keptSel
            in
            ( { model
                | rows = rows2
                , sprintCount = sCount
                , pi = ctx
                , filters = filters2
              }
            , Cmd.none
            )



-- HELPER
-- toSample :
--     { features : List { id : Int, title : String, state : String, areaPath : String, iterationPath : String, tags : List String }
--     , stories : List { id : Int, title : String, state : String, areaPath : String, iterationPath : String, parentId : Int }
--     }
--     -> Ado.Sample
-- toSample payload =
--     { features =
--         List.map
--             (\f -> { id = f.id, title = f.title, iterationPath = f.iterationPath, areaPath = f.areaPath, state = f.state, tags = f.tags })
--             payload.features
--     , stories =
--         List.map
--             (\s -> { id = s.id, title = s.title, parentId = s.parentId, iterationPath = s.iterationPath, state = s.state, areaPath = s.areaPath })
--             payload.stories
--     }
-- VIEW


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        toggles =
            { showTests = model.config.tags.enableTests }

        selectedTags : List String
        selectedTags =
            model.filters.sel.includeTags |> Set.toList

        rowsFiltered : List Feature
        rowsFiltered =
            case selectedTags of
                [] ->
                    model.rows

                _ ->
                    let
                        hasAll sel r =
                            List.all (\t -> List.member t r.tags) sel
                    in
                    List.filter (hasAll selectedTags) model.rows

        gridModel =
            model
                |> GT.fromApp
                |> (\g -> { g | rows = rowsFiltered })

        noRowsMsg : Html msg
        noRowsMsg =
            div
                [ A.class "mt-6 text-center text-slate-400 italic" ]
                [ text "No features match selected tags" ]

        -- override rows for view
    in
    div [ A.class ("w-full h-screen p-6" ++ GV.appBgColor) ]
        [ div [ A.class "text-2xl font-bold" ] [ text "Sprint Planner" ]
        , FV.view model.config (not (List.isEmpty model.rows)) model.filters |> Html.map Filters
        , if List.isEmpty rowsFiltered then
            noRowsMsg

          else
            GV.view toggles gridModel |> Html.map Grid
        ]
