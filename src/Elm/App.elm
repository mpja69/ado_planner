module App exposing (main)

import Browser
import Components.Rails exposing (..)
import Data.Ado as Ado
import Data.Translate as T
import Grid.Logic as GL
import Grid.Msg as GM
import Grid.Types as GT
import Grid.Update as GU
import Grid.View as GV
import Html exposing (Html, div, span, text)
import Html.Attributes as A
import Html.Events
import Json.Decode
import Status exposing (..)
import Types exposing (..)
import Ui exposing (UiSize(..), cardToneForStatus, statusPill, testChipView, testStripView, warnBadge)



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
    }


init : Model
init =
    let
        -- choose the PI and sprint labels (fake for now)
        sprintNames =
            List.map (\n -> "Sprint " ++ String.fromInt n) (List.range 1 5)

        ctx : T.PiContext
        ctx =
            T.buildPi "Contoso\\ART\\PI-2025-1" sprintNames

        rowsFromAdo : List Feature
        rowsFromAdo =
            T.translate ctx Ado.sample
    in
    { sprintCount = List.length sprintNames
    , rows = rowsFromAdo
    , draggingDelivery = Nothing
    , hoverDeliverySprint = Nothing
    , draggingStory = Nothing
    , hoverStorySprint = Nothing
    , pi = ctx
    , outbox = []
    }



-- UPDATE


type Msg
    = NoOp
    | Grid GM.Msg



--     ToggleTest Int TestKind
-- | DeliveryDragStart Int
-- | DeliveryDragEnd
-- | HoverDelivery (Maybe Int)
-- | DeliveryDrop Int
-- | StoryDragStart Int
-- | StoryDragEnd
-- | HoverStory (Maybe Int)
-- | StoryDrop Int


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



-- -- ===== Feature delivery drag =====
-- DeliveryDragStart featureId ->
--     { model | draggingDelivery = Just featureId, hoverDeliverySprint = Nothing }
--
-- DeliveryDragEnd ->
--     { model | draggingDelivery = Nothing, hoverDeliverySprint = Nothing }
--
-- HoverDelivery maybeIx ->
--     { model | hoverDeliverySprint = maybeIx }
--
-- DeliveryDrop newSprintIx ->
--     case model.draggingDelivery of
--         Nothing ->
--             model
--
--         Just fid ->
--             let
--                 -- update the delivery in rows
--                 updateRow r =
--                     if r.featureId == fid then
--                         { r | iteration = InPI newSprintIx }
--
--                     else
--                         r
--
--                 newRows =
--                     List.map updateRow model.rows
--
--                 -- enqueue ADO intent
--                 newOutbox =
--                     SetFeatureIteration { featureId = fid, toSprintIx = newSprintIx }
--                         :: model.outbox
--             in
--             { model
--                 | rows = newRows
--                 , draggingDelivery = Nothing
--                 , hoverDeliverySprint = Nothing
--                 , outbox = newOutbox
--             }
--
-- -- ===== Story drag =====
-- StoryDragStart storyId ->
--     { model | draggingStory = Just storyId, hoverStorySprint = Nothing }
--
-- StoryDragEnd ->
--     { model | draggingStory = Nothing, hoverStorySprint = Nothing }
--
-- HoverStory maybeIx ->
--     { model | hoverStorySprint = maybeIx }
--
-- StoryDrop newSprintIx ->
--     case model.draggingStory of
--         Nothing ->
--             model
--
--         Just sid ->
--             let
--                 -- update targeted story's iteration to InPI newSprintIx
--                 updateStory s =
--                     if s.id == sid then
--                         { s | iteration = InPI newSprintIx }
--
--                     else
--                         s
--
--                 updateRow r =
--                     { r | stories = List.map updateStory r.stories }
--
--                 newRows =
--                     List.map updateRow model.rows
--
--                 newOutbox =
--                     SetStoryIteration { storyId = sid, toSprintIx = newSprintIx }
--                         :: model.outbox
--             in
--             { model
--                 | rows = newRows
--                 , draggingStory = Nothing
--                 , hoverStorySprint = Nothing
--                 , outbox = newOutbox
--             }
--
-- -- ===== Tests toggle (tags) =====
-- ToggleTest featureId kind ->
--     let
--         newRows =
--             List.map (GL.toggleTest featureId kind) model.rows
--
--         updated =
--             List.filter (\r -> r.featureId == featureId) newRows
--                 |> List.head
--
--         newOutbox =
--             case updated of
--                 Just r2 ->
--                     SetFeatureTags
--                         { featureId = featureId
--                         , sit = r2.tests.sit
--                         , uat = r2.tests.uat
--                         , e2e = r2.tests.e2e
--                         }
--                         :: model.outbox
--
--                 Nothing ->
--                     model.outbox
--     in
--     { model | rows = newRows, outbox = newOutbox }
-- HELPERS


attrsIf : Bool -> List (Html.Attribute Msg) -> List (Html.Attribute Msg)
attrsIf cond attrs =
    if cond then
        attrs

    else
        []



-- VIEW


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init, update = update, view = view }


view : Model -> Html Msg
view model =
    div [ A.class "w-full h-screen p-6" ]
        [ div [ A.class "text-2xl font-bold mb-4" ] [ text "Sprint Planner" ]
        , GV.view (GT.fromApp model) |> Html.map Grid
        ]



-- gridView : Model -> Html Msg
-- gridView model =
--     let
--         unscheduled =
--             GL.hasUnscheduledItems model.rows
--
--         leftColWidth =
--             if unscheduled then
--                 "220px"
--
--             else
--                 "12px"
--
--         templateCols =
--             leftColWidth ++ " repeat(" ++ String.fromInt model.sprintCount ++ ", minmax(0, 1fr))"
--     in
--     div
--         [ A.style "display" "grid"
--         , A.style "grid-template-columns" templateCols
--         , A.class "gap-2"
--         ]
--         (headerRow unscheduled model.sprintCount
--             :: List.concatMap (featureRowView model) model.rows
--         )
--
--
-- headerRow : Bool -> Int -> Html Msg
-- headerRow unscheduled n =
--     let
--         headCell s =
--             div [ A.class "text-xs font-semibold uppercase tracking-wide text-slate-600 px-2 py-1" ] [ text s ]
--
--         mutedCell =
--             -- empty/transparent cell so the grid stays aligned
--             div [ A.class "px-0 py-0" ] []
--     in
--     div [ A.class "contents" ]
--         ((if unscheduled then
--             headCell "Unscheduled"
--
--           else
--             mutedCell
--          )
--             :: List.map (\i -> headCell ("Sprint " ++ String.fromInt i)) (List.range 1 n)
--         )
--
--
-- featureRowView : Model -> Feature -> List (Html Msg)
-- featureRowView model row =
--     let
--         can =
--             GL.canInteract row
--
--         --mode
--         warnKind : FeatureWarn
--         warnKind =
--             GL.selectWarnKind row
--
--         isUnscheduledFeature =
--             GL.isUnscheduled row.iteration
--
--         isUnscheduledStories =
--             not (List.isEmpty (GL.unscheduledStories row))
--
--         isUnscheduledItems =
--             isUnscheduledFeature || isUnscheduledStories
--
--         emptyFeatureCell : Html Msg
--         emptyFeatureCell =
--             div [ A.class "p-0 m-0" ] []
--
--         -- INSIDE featureRowView, in the local `featureCell`:
--         fullFeatureCell : Html Msg
--         fullFeatureCell =
--             let
--                 baseCls : String
--                 baseCls =
--                     "px-3 py-2 rounded-xl border font-medium sticky left-0 z-10 transition "
--                         ++ " border-slate-200"
--
--                 -- “Unscheduled” tray (Missing / WholePI / OutsidePI stories)
--                 tray : List (Html Msg)
--                 tray =
--                     if isUnscheduledStories then
--                         [ div [ A.class "mt-1 rounded-lg border border-slate-200 bg-white/70 p-1" ]
--                             [ div [ A.class "text-[9px] text-slate-500 uppercase" ] [ text "Stories" ]
--                             , div [ A.class "flex flex-col gap-1" ]
--                                 (List.map (\s -> storyCard False can s) (GL.unscheduledStories row))
--                             ]
--                         ]
--
--                     else
--                         []
--
--                 maybeFeatureCard : List (Html Msg)
--                 maybeFeatureCard =
--                     if isUnscheduledFeature then
--                         [ featureCard False warnKind can row ]
--
--                     else
--                         []
--
--                 maybeText : List (Html Msg)
--                 maybeText =
--                     if isUnscheduledFeature then
--                         [ span [ A.class "text-[9px] text-slate-500 uppercase" ] [ text "Feature" ] ]
--
--                     else
--                         []
--             in
--             div
--                 [ A.class baseCls
--                 , A.style "backdrop-filter" "blur(2px)"
--                 ]
--                 (div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
--                     maybeText
--                     :: maybeFeatureCard
--                     ++ tray
--                 )
--
--         cells : List (Html Msg)
--         cells =
--             List.map (\ix -> sprintCell model row ix)
--                 (List.range 1 model.sprintCount)
--     in
--     (if isUnscheduledItems then
--         fullFeatureCell
--
--      else
--         emptyFeatureCell
--     )
--         :: cells
--
--
-- sprintCell : Model -> Feature -> Int -> Html Msg
-- sprintCell model row ix =
--     let
--         can : Bool
--         can =
--             GL.canInteract row
--
--         -- 1) Content in this cell -----------------------------------
--         storiesHere : List Story
--         storiesHere =
--             row.stories
--                 |> List.filter
--                     (\s ->
--                         case s.iteration of
--                             InPI k ->
--                                 k == ix
--
--                             _ ->
--                                 False
--                     )
--
--         isDeliveryHere : Bool
--         isDeliveryHere =
--             case row.iteration of
--                 InPI k ->
--                     k == ix
--
--                 _ ->
--                     False
--
--         -- 2) Drag target validity (valid row)
--         validStoryTarget : Bool
--         validStoryTarget =
--             case model.draggingStory of
--                 Nothing ->
--                     False
--
--                 Just sid ->
--                     let
--                         belongsHere =
--                             List.any (\s -> s.id == sid) row.stories
--
--                         withinDeliveryGate =
--                             case row.iteration of
--                                 InPI d ->
--                                     ix <= d
--
--                                 _ ->
--                                     True
--                     in
--                     belongsHere && withinDeliveryGate
--
--         validDeliveryTarget : Bool
--         validDeliveryTarget =
--             case model.draggingDelivery of
--                 Nothing ->
--                     False
--
--                 Just fid ->
--                     -- Only this feature's delivery, and only at/after last story sprint
--                     if fid /= row.featureId then
--                         False
--
--                     else
--                         ix >= GL.lastStorySprint row
--
--         -- 3) Hover state (valid sprint && valid row)
--         hoveredForStory : Bool
--         hoveredForStory =
--             model.hoverStorySprint == Just ix && validStoryTarget
--
--         hoveredForDelivery : Bool
--         hoveredForDelivery =
--             model.hoverDeliverySprint == Just ix && validDeliveryTarget
--
--         -- 4) Visuals -------------------------------------------------
--         baseClass : String
--         baseClass =
--             "min-w-0 px-3 py-2 rounded-xl border min-h-[84px] flex flex-col gap-1 transition "
--
--         -- ++ toneCls
--         validCls : String
--         validCls =
--             if can && (validStoryTarget || validDeliveryTarget) then
--                 " border-indigo-300 border-dashed "
--
--             else
--                 " border-slate-200 "
--
--         hoverCls : String
--         hoverCls =
--             if can && (hoveredForStory || hoveredForDelivery) then
--                 " ring-2 ring-indigo-400 bg-indigo-50/40 "
--
--             else
--                 ""
--
--         forbidClass : String
--         forbidClass =
--             if can then
--                 ""
--
--             else
--                 " cursor-not-allowed"
--
--         -- 5) Handlers (built from validity; attached by the ROW gate)
--         storyHandlers : List (Html.Attribute Msg)
--         storyHandlers =
--             if validStoryTarget then
--                 [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverStory (Just ix)))
--                 , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverStory (Just ix), True ))
--                 , Html.Events.on "dragleave" (Json.Decode.succeed (HoverStory Nothing))
--                 , Html.Events.on "drop" (Json.Decode.succeed (StoryDrop ix))
--                 ]
--
--             else
--                 []
--
--         deliveryHandlers : List (Html.Attribute Msg)
--         deliveryHandlers =
--             if validDeliveryTarget then
--                 [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverDelivery (Just ix)))
--                 , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverDelivery (Just ix), True ))
--                 , Html.Events.on "dragleave" (Json.Decode.succeed (HoverDelivery Nothing))
--                 , Html.Events.on "drop" (Json.Decode.succeed (DeliveryDrop ix))
--                 ]
--
--             else
--                 []
--
--         -- 6) Ghosting ------------------------------------------------
--         -- Ghost the original story chip when that specific story is being dragged
--         isGhostStory : Story -> Bool
--         isGhostStory s =
--             case model.draggingStory of
--                 Just sid ->
--                     sid
--                         == s.id
--                         && (case s.iteration of
--                                 InPI k ->
--                                     k == ix
--
--                                 _ ->
--                                     False
--                            )
--
--                 Nothing ->
--                     False
--
--         -- Ghost the original feature delivery card in its original cell
--         isGhostDelivery : Bool
--         isGhostDelivery =
--             case ( model.draggingDelivery, row.iteration ) of
--                 ( Just fid, InPI k ) ->
--                     fid == row.featureId && k == ix
--
--                 _ ->
--                     False
--
--         -- 7) Content helpers -----------------------------------------
--         emptyHint : List (Html Msg)
--         emptyHint =
--             if List.isEmpty storiesHere && not isDeliveryHere then
--                 [ div [ A.class "text-[11px] text-slate-400 italic" ] [ text "–" ] ]
--
--             else
--                 []
--     in
--     div
--         (A.class (baseClass ++ validCls ++ hoverCls ++ forbidClass)
--             :: attrsIf can (storyHandlers ++ deliveryHandlers)
--         )
--         (List.map (\s -> storyCard (isGhostStory s) can s) storiesHere
--             ++ (if isDeliveryHere then
--                     [ featureCard isGhostDelivery (GL.selectWarnKind row) can row ]
--
--                 else
--                     []
--                )
--             ++ emptyHint
--         )
--
--
-- storyCard : Bool -> Bool -> Story -> Html Msg
-- storyCard isGhost can s =
--     let
--         ghostClass =
--             if isGhost then
--                 " opacity-30 saturate-50"
--
--             else
--                 ""
--
--         dragAttrs : List (Html.Attribute Msg)
--         dragAttrs =
--             if can then
--                 [ A.attribute "draggable" "true"
--                 , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
--                 , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
--                 , Html.Events.on "dragstart" (Json.Decode.succeed (StoryDragStart s.id))
--                 , Html.Events.on "dragend" (Json.Decode.succeed StoryDragEnd)
--                 ]
--
--             else
--                 []
--     in
--     div
--         ([ A.class
--             ("min-w-0 flex items-center justify-between gap-2 pl-1 pr-2 py-1 rounded-lg border select-none "
--                 ++ cardToneForStatus s.status
--                 ++ ghostClass
--             )
--          , A.title s.title
--          ]
--             ++ dragAttrs
--         )
--         [ -- LEFT: rail + title (truncates nicely)
--           div [ A.class "min-w-0 inline-flex items-center gap-1.5" ]
--             (if can then
--                 [ dotRailPx (railSpec Small Regular)
--                 , span [ A.class "font-medium truncate text-[13px]" ] [ text s.title ]
--                 ]
--
--              else
--                 [ span [ A.class "font-medium truncate text-[13px]" ] [ text s.title ] ]
--             )
--
--         --  Don't need iteration warning?!?!
--         --, div [ A.class "flex items-center shrink-0" ] [ warnIterationBadge Tiny s.iteration ]
--         ]
--
--
-- featureCard : Bool -> FeatureWarn -> Bool -> Feature -> Html Msg
-- featureCard isGhost warnKind can row =
--     let
--         -- visuals
--         ghostClass =
--             if isGhost then
--                 " opacity-30 saturate-50"
--
--             else
--                 ""
--
--         dragAttrs : List (Html.Attribute Msg)
--         dragAttrs =
--             if can then
--                 [ A.attribute "draggable" "true"
--                 , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
--                 , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
--                 , Html.Events.on "dragstart" (Json.Decode.succeed (DeliveryDragStart row.featureId))
--                 , Html.Events.on "dragend" (Json.Decode.succeed DeliveryDragEnd)
--                 ]
--
--             else
--                 []
--     in
--     div
--         (A.class
--             ("mt-1 rounded-xl border select-none overflow-hidden "
--                 ++ cardToneForStatus row.status
--                 ++ ghostClass
--             )
--             :: dragAttrs
--         )
--         [ -- ROW: rail + content
--           div [ A.class "flex items-stretch" ]
--             [ -- left rail (keep your existing rail call; you said you refactored it)
--               if can then
--                 div [ A.class "ml-2 self-center" ]
--                     [ dotRailPx (railSpec Medium Regular) ]
--
--               else
--                 div [ A.class "ml-2" ] []
--             , -- content
--               div [ A.class "flex-1 pl-1 pr-2 py-2 min-w-0" ]
--                 [ -- row 1: title + status + warn
--                   div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
--                     [ span [ A.class "text-[13px] font-semibold truncate" ] [ text row.title ]
--                     ]
--                 , -- row 2: delivery + test chips
--                   if warnKind == NoWarn then
--                     div [ A.class "mt-1 flex items-center justify-between gap-2 min-w-0" ]
--                         [ statusPill Tiny row.status
--                         , testStrip can row.featureId row.tests
--                         ]
--
--                   else
--                     div [ A.class "flex items-center gap-1 shrink-0" ]
--                         [ warnBadge Tiny warnKind ]
--                 ]
--             ]
--         ]
--
--
-- testStrip : Bool -> Int -> Tests -> Html Msg
-- testStrip can featureId tests =
--     let
--         mk label active kind =
--             testChipView Tiny
--                 { label = label, active = active }
--                 (if can then
--                     [ Html.Events.onClick (ToggleTest featureId kind) ]
--
--                  else
--                     []
--                 )
--
--         chips =
--             [ mk "SIT" tests.sit SIT
--             , mk "UAT" tests.uat UAT
--             , mk "E2E" tests.e2e E2E
--             ]
--     in
--     testStripView Tiny chips
