module App exposing (main)

import Browser
import Components.Rails exposing (..)
import Config
import Data.Ado as Ado
import Data.Filter as F
import Data.Translate as T
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

import Data.Seed as Seed
import Filters.AreaSelector as AS

-- TODO:
-- Add planning of of stories without Feature
-- Maybe have specific rows for different Type of Work
--    Technical Improvements
--    Test Automation
-- Add a Settings where you can
--    Map tests -> Tags
--    Map ToW -> Tags
-- Add checking for AreaPath
--    Are the stories in the right place?
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
    }


init : Model
init =
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
        (as1, _) =
            AS.update (AS.ReplaceAreas Seed.areas) filters0.areaSel

        (as2, _) =
            AS.update (AS.ReplaceFavorites []) as1

        filters1 : FT.Model
        filters1 =
            { filters0 | areaSel = as2 }
    in
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
    }


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

                        sprintNames =
                            Seed.sprintNamesFor piRoot

                        ctx =
                            T.buildPi piRoot sprintNames

                        rows2 =
                            T.translate model.config ctx sub

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
                        | rows = rows2
                        , sprintCount = List.length sprintNames
                        , outbox = []
                        , filters = filters2
                    }

                _ ->
                    -- not handled here; leave model as-is
                    m
    in
    List.foldl step model model.outbox



-- UPDATE


type Msg
    = Grid GM.Msg
    | Filters FM.Msg
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Grid gm ->
            let
                -- convert App.Model -> Grid.Model, then back by copying fields
                ( g2, intents ) =
                    GU.update gm (GT.fromApp model)
            in
            { model
                | sprintCount = g2.sprintCount
                , rows = g2.rows
                , draggingDelivery = g2.draggingDelivery
                , hoverDeliverySprint = g2.hoverDeliverySprint
                , draggingStory = g2.draggingStory
                , hoverStorySprint = g2.hoverStorySprint
                , outbox = intents ++ model.outbox
            }

        Filters fmsg ->
            let
                filters2 =
                    FU.update fmsg model.filters

                newIntents =
                    case ( filters2.sel.area, filters2.sel.iteration ) of
                        ( Just art, Just pi ) ->
                                [ FetchFeatures { artAreaPath = art, piRoot = pi } ]

                        _ ->
                            []
            in
            { model
                | filters = filters2
                , outbox = model.outbox ++ newIntents
            }
                |> runIntents



-- VIEW


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init, update = update, view = view }


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
