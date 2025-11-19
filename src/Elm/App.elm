port module App exposing (main)

import Browser
import Config
import Data.Ado as Ado
import Data.Filter as F
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
import Settings.Msg as SM
import Settings.Types as ST
import Settings.Update as SU
import Settings.View as SV
import Status exposing (..)
import Types exposing (..)
import Ui.Rails exposing (..)
import Ui.Theme exposing (UiSize(..))



-- PORTS


port requestAreas : () -> Cmd msg


port receiveAreas : (List { id : String, name : String } -> msg) -> Sub msg


port requestIterations : () -> Cmd msg


port receivePiMeta :
    (List { root : String, sprintNames : List String } -> msg)
    -> Sub msg


port requestData : { area : String, pi : String } -> Cmd msg


port receiveData :
    ({ features : List Ado.AdoFeature
     , stories : List Ado.AdoStory
     }
     -> msg
    )
    -> Sub msg


port receiveAreaFavorites : (List String -> msg) -> Sub msg


port sendSetIteration : { id : Int, iterationPath : String } -> Cmd msg


port sendUpdateTests :
    { id : Int
    , sit : Bool
    , uat : Bool
    , e2e : Bool
    , sitTag : String
    , uatTag : String
    , e2eTag : String
    }
    -> Cmd msg


port openWorkItem : Int -> Cmd msg



-- TODO:
-- Add planning of of stories without Feature


type alias Model =
    { grid : GT.Model
    , pi : T.PiContext
    , outbox : List AdoCmd
    , filters : FT.Model
    , settings : ST.Model
    , config : Config.Config
    , piSprintNames : Dict String (List String)
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        cfg =
            Config.default

        model =
            { grid = GT.empty
            , pi = T.emptyPi
            , outbox = []
            , filters = FT.init
            , config = cfg
            , settings = ST.fromConfig cfg
            , piSprintNames = Dict.empty
            }
    in
    ( model, Cmd.batch [ requestAreas (), requestIterations () ] )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveAreas GotAreas
        , receivePiMeta GotPiMeta
        , receiveData GotData
        , receiveAreaFavorites GotAreaFavorites
        ]



-- MESSAGES


type Msg
    = Grid GM.Msg
    | Filters FM.Msg
    | SettingsMsg SM.Msg
    | GotAreas (List { id : String, name : String })
    | GotPiMeta (List { root : String, sprintNames : List String })
    | GotData { features : List Ado.AdoFeature, stories : List Ado.AdoStory }
    | GotAreaFavorites (List String)
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SettingsMsg smsg ->
            let
                ( settings2, cfgPatch ) =
                    SU.update smsg model.settings

                config2 =
                    cfgPatch model.config
            in
            ( { model | settings = settings2, config = config2 }
            , Cmd.none
            )

        GotAreaFavorites favIds ->
            let
                ( as2, _ ) =
                    AS.update
                        (AS.ReplaceFavorites favIds)
                        model.filters.areaSel

                filters1 =
                    model.filters
            in
            ( { model | filters = { filters1 | areaSel = as2 } }
            , Cmd.none
            )

        GotAreas miniAreas ->
            let
                -- 1) Mappa till det format AreaSelector vill ha
                areasRaw : List { id : String, name : String }
                areasRaw =
                    List.map (\r -> { id = r.id, name = r.name }) miniAreas

                -- 2) Sortera alfabetiskt på name (case-insensitive)
                areasSorted : List { id : String, name : String }
                areasSorted =
                    areasRaw
                        |> List.sortBy (\a -> String.toLower a.name)

                -- 3) Skicka in den sorterade listan till AreaSelector
                ( as2, chosen ) =
                    AS.update (AS.ReplaceAreas (List.map (\r -> { id = r.id, name = r.name }) areasSorted)) model.filters.areaSel

                keepSel =
                    case ( model.filters.sel.area, chosen ) of
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

        Grid gm ->
            let
                ( g2, intents ) =
                    GU.update gm model.grid

                model2 =
                    { model | grid = g2 }

                cmdUpdates =
                    intents
                        |> List.filterMap (adoCmdToCmd model2)
                        |> Cmd.batch
            in
            ( model2, cmdUpdates )

        Filters fmsg ->
            let
                filters2 =
                    FU.update fmsg model.filters

                cmdFetch =
                    case ( filters2.sel.area, filters2.sel.iteration ) of
                        ( Just art, Just pi ) ->
                            requestData { area = art, pi = pi }

                        _ ->
                            Cmd.none
            in
            ( { model | filters = filters2 }, cmdFetch )

        GotPiMeta rows ->
            let
                dictNames : Dict String (List String)
                dictNames =
                    List.foldl
                        (\r acc -> Dict.insert r.root r.sprintNames acc)
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
            ( { model | piSprintNames = dictNames, filters = filters2 }, Cmd.none )

        GotData payload ->
            let
                -- Data is already filtered by (area, pi) in TS, so we can translate directly
                sample =
                    { features = payload.features
                    , stories = payload.stories
                    }

                -- CTX
                piRoot =
                    Maybe.withDefault "" model.filters.sel.iteration

                sprintNamesFromPi =
                    Dict.get piRoot model.piSprintNames
                        |> Maybe.withDefault []

                -- om av ngn anledning listan saknas, fallback
                sprintNames =
                    if List.isEmpty sprintNamesFromPi then
                        -- t.ex. default 5, eller [] om du vill
                        List.map (\n -> "Sprint " ++ String.fromInt n) (List.range 1 5)
                            |> Debug.log "Generated SprintNames: "

                    else
                        sprintNamesFromPi
                            |> Debug.log "Existing SprintNames: "

                ctx =
                    T.buildPi piRoot sprintNames

                -- GRID
                rows2 =
                    T.translate model.config ctx sample

                grid1 =
                    model.grid

                grid2 =
                    { grid1
                        | rows = rows2
                        , sprintCount = List.length sprintNames
                    }

                -- FILTERS
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
                | grid = grid2
                , pi = ctx
                , filters = filters2
              }
            , Cmd.none
            )



-- HELPER


filterByTeam : Maybe String -> List Feature -> List Feature
filterByTeam maybeTeam rows =
    case maybeTeam of
        Nothing ->
            rows

        Just teamTag ->
            List.filter (\r -> List.member teamTag r.tags) rows


filterByTags : FT.TagMode -> List String -> List Feature -> List Feature
filterByTags mode tags rows =
    case tags of
        [] ->
            rows

        sel ->
            let
                hasAll row =
                    List.all (\t -> List.member t row.tags) sel

                hasAny row =
                    List.any (\t -> List.member t row.tags) sel
            in
            case mode of
                FT.TagAnd ->
                    List.filter hasAll rows

                FT.TagOr ->
                    List.filter hasAny rows


adoCmdToCmd : Model -> AdoCmd -> Maybe (Cmd Msg)
adoCmdToCmd model intent =
    case intent of
        SetIteration payload ->
            case T.iterationPathForSprintIx payload.toSprintIx model.pi of
                Just path ->
                    Just <|
                        sendSetIteration
                            { id = payload.id
                            , iterationPath = path
                            }

                Nothing ->
                    Nothing

        SetFeatureTags payload ->
            let
                tcfg =
                    model.config.tags.testTags

                _ =
                    Debug.log "SetFeatureTags → payload"
                        ( payload.sit, payload.uat, payload.e2e )
            in
            Just <|
                sendUpdateTests
                    { id = payload.featureId
                    , sit = payload.sit
                    , uat = payload.uat
                    , e2e = payload.e2e
                    , sitTag = tcfg.sit
                    , uatTag = tcfg.uat
                    , e2eTag = tcfg.e2e
                    }

        OpenWorkItem id ->
            Just (openWorkItem id)

        _ ->
            Nothing



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
        toggles : GV.Toggles
        toggles =
            { showTests = model.config.tags.enableTests
            , editableTests = model.config.tags.editableTests
            }

        selectedTeam : Maybe String
        selectedTeam =
            model.filters.sel.team

        selectedTags : List String
        selectedTags =
            model.filters.sel.includeTags |> Set.toList

        tagMode : FT.TagMode
        tagMode =
            model.filters.sel.tagMode

        rowsFiltered : List Feature
        rowsFiltered =
            model.grid.rows
                |> filterByTeam selectedTeam
                |> filterByTags tagMode selectedTags

        gridModel : GT.Model
        gridModel =
            model.grid
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
        , SV.view model.settings |> Html.map SettingsMsg
        , FV.view model.config model.filters |> Html.map Filters
        , if List.isEmpty rowsFiltered then
            noRowsMsg

          else
            GV.view toggles gridModel |> Html.map Grid
        ]
