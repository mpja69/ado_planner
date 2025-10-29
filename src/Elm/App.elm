module App exposing (main)

import Browser
import Components.Rails exposing (..)
import Data.Ado as Ado
import Data.Translate as T
import Dict exposing (Dict)
import Html exposing (Html, button, div, span, text)
import Html.Attributes as A
import Html.Events
import Json.Decode
import Set exposing (Set)
import Time exposing (Posix)
import Types exposing (..)
import Ui exposing (UiSize(..), cardToneForStatus, statusPill, storyIterationBadge, testChipView, testStripView, warnBadge)



-- TODO:
-- Add planning of of stories without Feature
-- Maybe have specific rows for our decided ToW, Type of Work
--    Technical Improvements
--    Test Automation
-- Add a Settings where you can
--    Map tests -> Tags
--    Map ToW -> Tags
-- Add checking for AreaPath
--    Are the stories in the right place?


type alias Model =
    { sprintCount : Int
    , rows : List Feature
    , draggingDelivery : Maybe Int
    , hoverDeliverySprint : Maybe Int
    , draggingStory : Maybe Int
    , hoverStorySprint : Maybe Int
    , pi : T.PiContext
    , outbox : List AdoCmd
    , unlocked : Set Int -- featureIds that are temporarily expanded/unlocked
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
    , unlocked = Set.empty
    }



-- UPDATE


type Msg
    = ToggleTest Int TestKind
    | DeliveryDragStart Int
    | DeliveryDragEnd
    | HoverDelivery (Maybe Int)
    | DeliveryDrop Int
    | StoryDragStart Int
    | StoryDragEnd
    | HoverStory (Maybe Int)
    | StoryDrop Int
    | UnlockRow Int -- NEW
    | LockRow Int -- NEW (used if you add a collapse control later)
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        -- ===== Feature delivery drag =====
        DeliveryDragStart featureId ->
            { model | draggingDelivery = Just featureId, hoverDeliverySprint = Nothing }

        DeliveryDragEnd ->
            { model | draggingDelivery = Nothing, hoverDeliverySprint = Nothing }

        HoverDelivery maybeIx ->
            { model | hoverDeliverySprint = maybeIx }

        DeliveryDrop newSprintIx ->
            case model.draggingDelivery of
                Nothing ->
                    model

                Just fid ->
                    let
                        -- update the delivery in rows
                        updateRow r =
                            if r.featureId == fid then
                                { r | iteration = InPI newSprintIx }

                            else
                                r

                        newRows =
                            List.map updateRow model.rows

                        -- enqueue ADO intent
                        newOutbox =
                            SetFeatureIteration { featureId = fid, toSprintIx = newSprintIx }
                                :: model.outbox
                    in
                    { model
                        | rows = newRows
                        , draggingDelivery = Nothing
                        , hoverDeliverySprint = Nothing
                        , outbox = newOutbox
                    }

        -- ===== Story drag =====
        StoryDragStart storyId ->
            { model | draggingStory = Just storyId, hoverStorySprint = Nothing }

        StoryDragEnd ->
            { model | draggingStory = Nothing, hoverStorySprint = Nothing }

        HoverStory maybeIx ->
            { model | hoverStorySprint = maybeIx }

        StoryDrop newSprintIx ->
            case model.draggingStory of
                Nothing ->
                    model

                Just sid ->
                    let
                        -- update targeted story's iteration to InPI newSprintIx
                        updateStory s =
                            if s.id == sid then
                                { s | iteration = InPI newSprintIx }

                            else
                                s

                        updateRow r =
                            { r | stories = List.map updateStory r.stories }

                        newRows =
                            List.map updateRow model.rows

                        newOutbox =
                            SetStoryIteration { storyId = sid, toSprintIx = newSprintIx }
                                :: model.outbox
                    in
                    { model
                        | rows = newRows
                        , draggingStory = Nothing
                        , hoverStorySprint = Nothing
                        , outbox = newOutbox
                    }

        -- ===== Tests toggle (tags) =====
        ToggleTest featureId kind ->
            let
                newRows =
                    List.map (toggleTest featureId kind) model.rows

                updated =
                    List.filter (\r -> r.featureId == featureId) newRows
                        |> List.head

                newOutbox =
                    case updated of
                        Just r2 ->
                            SetFeatureTags
                                { featureId = featureId
                                , sit = r2.tests.sit
                                , uat = r2.tests.uat
                                , e2e = r2.tests.e2e
                                }
                                :: model.outbox

                        Nothing ->
                            model.outbox
            in
            { model | rows = newRows, outbox = newOutbox }

        -- NEW: expand/unlock a closed row
        UnlockRow fid ->
            { model | unlocked = Set.insert fid model.unlocked }

        -- NEW: collapse/lock (if you later add a button to re-compact)
        LockRow fid ->
            { model | unlocked = Set.remove fid model.unlocked }



-- HELPERS


toggleTest : Int -> TestKind -> Feature -> Feature
toggleTest fid kind row =
    if row.featureId /= fid then
        row

    else
        let
            t =
                row.tests

            new =
                case kind of
                    SIT ->
                        { t | sit = not t.sit }

                    UAT ->
                        { t | uat = not t.uat }

                    E2E ->
                        { t | e2e = not t.e2e }
        in
        { row | tests = new }


lastStorySprint : Feature -> Int
lastStorySprint row =
    row.stories
        |> List.filterMap
            (\s ->
                case s.iteration of
                    InPI ix ->
                        Just ix

                    _ ->
                        Nothing
            )
        |> List.maximum
        |> Maybe.withDefault 1


selectWarnKind : Feature -> FeatureWarn
selectWarnKind row =
    if row.status == Done then
        if List.any (\s -> s.status /= Done) row.stories then
            WarnStoriesNotDone

        else
            NoWarn

    else
    -- NEW: Feature kvar i Todo men någon story är > Todo
    if
        row.status == Todo && hasStoryBeyondTodo row
    then
        WarnFeatureLagging

    else
        case featureDeliverySprint row of
            Nothing ->
                WarnNeedsDelivery

            Just _ ->
                if hasStoryAfterDelivery row then
                    WarnAfter

                else
                    NoWarn



-- Is there ANY unfinished story on the row?


hasUnfinishedStories : Feature -> Bool
hasUnfinishedStories row =
    row.stories |> List.any (\s -> s.status /= Done)



-- Is there ANY unfinished story specifically in sprint ix?


hasUnfinishedStoriesInSprint : Feature -> Int -> Bool
hasUnfinishedStoriesInSprint row ix =
    row.stories
        |> List.any
            (\s ->
                case s.iteration of
                    InPI k ->
                        k == ix && s.status /= Done

                    _ ->
                        False
            )


unscheduledStories : Feature -> List Story
unscheduledStories row =
    row.stories
        |> List.filter
            (\s ->
                case s.iteration of
                    InPI _ ->
                        False

                    -- show Missing / WholePI / OutsidePI
                    _ ->
                        True
            )



-- Determine mode of a feature row based on its status and unlock state


rowMode : Model -> Feature -> RowMode
rowMode model row =
    if row.status == Done then
        if Set.member row.featureId model.unlocked then
            DoneExpanded

        else
            DoneCompact

    else
        Open



-- Centralized interaction rule


canInteract : RowMode -> Bool
canInteract mode =
    case mode of
        Open ->
            True

        -- 🔒 Expanded-but-done rows are read-only
        DoneExpanded ->
            False

        -- compact rows are also read-only
        DoneCompact ->
            False


rowTintClass : Model -> Feature -> String
rowTintClass model row =
    case rowMode model row of
        Open ->
            " bg-white"

        DoneExpanded ->
            " bg-emerald-50/50"

        DoneCompact ->
            " bg-emerald-50/50"



-- Only include attributes when `cond` is true


attrsIf : Bool -> List (Html.Attribute Msg) -> List (Html.Attribute Msg)
attrsIf cond attrs =
    if cond then
        attrs

    else
        []



-- NEW: extract sprint index from an Iteration -------------------


iterationToSprint : Iteration -> Maybe Int
iterationToSprint itn =
    case itn of
        InPI ix ->
            Just ix

        _ ->
            Nothing



-- NEW: feature's delivery sprint (if set)


featureDeliverySprint : Feature -> Maybe Int
featureDeliverySprint f =
    iterationToSprint f.iteration



-- UPDATE: was using row.delivery; now uses featureDeliverySprint


hasStoryAfterDelivery : Feature -> Bool
hasStoryAfterDelivery row =
    case featureDeliverySprint row of
        Nothing ->
            False

        Just d ->
            row.stories
                |> List.any
                    (\s ->
                        case s.iteration of
                            InPI ix ->
                                ix > d

                            _ ->
                                False
                    )


hasStoryBeyondTodo : Feature -> Bool
hasStoryBeyondTodo row =
    List.any (\s -> s.status /= Todo) row.stories



-- ----------------------------------------------------------------
-- Stub: fill in using your PI boundaries/cadence later
-- closedSprintIx : Model -> FeatureRow -> Maybe Int
-- closedSprintIx model row =
--     case row.closedDate of
--         Nothing ->
--             Nothing
--
--         Just posix ->
--             dateToSprint model posix
-- dateToSprint : Model -> Posix -> Maybe Int
-- dateToSprint _ _ =
--     Nothing
-- VIEW


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init, update = update, view = view }


view : Model -> Html Msg
view model =
    div [ A.class "w-full h-screen p-6" ]
        [ div [ A.class "text-2xl font-bold mb-4" ] [ text "Sprint Planner" ]
        , gridView model
        ]


gridView : Model -> Html Msg
gridView model =
    let
        templateCols =
            "220px repeat(" ++ String.fromInt model.sprintCount ++ ", minmax(0, 1fr))"
    in
    div
        [ A.style "display" "grid"
        , A.style "grid-template-columns" templateCols
        , A.class "gap-2"
        ]
        (headerRow model.sprintCount
            :: List.concatMap (rowView model) model.rows
        )


rowView : Model -> Feature -> List (Html Msg)
rowView model row =
    case rowMode model row of
        DoneCompact ->
            featureRowViewDoneCompact model row

        _ ->
            featureRowView model row


headerRow : Int -> Html Msg
headerRow n =
    let
        headCell s =
            div [ A.class "text-xs font-semibold uppercase tracking-wide text-slate-600 px-2 py-1" ] [ text s ]
    in
    div [ A.class "contents" ]
        (headCell "Feature"
            :: List.map (\i -> headCell ("Sprint " ++ String.fromInt i)) (List.range 1 n)
        )


type HeaderLayout
    = ChevronLeft
    | ChevronRight


featureHeader : HeaderLayout -> RowMode -> Feature -> List (Html Msg) -> Html Msg
featureHeader layout mode row rightExtras =
    let
        titleCls =
            case mode of
                DoneCompact ->
                    "text-[11px] font-semibold truncate"

                _ ->
                    "text-[12px] font-semibold truncate"

        chevronView : Html Msg
        chevronView =
            case mode of
                DoneCompact ->
                    -- expand (▸)
                    span
                        [ A.class "inline-flex items-center justify-center w-5 h-5 rounded-md bg-white/60 border border-emerald-200 cursor-pointer select-none shrink-0"
                        , Html.Events.onClick (UnlockRow row.featureId)
                        , A.title "Expand row"
                        ]
                        [ text "▸" ]

                DoneExpanded ->
                    -- collapse (▾)
                    span
                        [ A.class "inline-flex items-center justify-center w-5 h-5 rounded-md bg-white/60 border border-emerald-200 cursor-pointer select-none shrink-0"
                        , Html.Events.onClick (LockRow row.featureId)
                        , A.title "Collapse row"
                        ]
                        [ text "▾" ]

                Open ->
                    text ""
    in
    div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
        (case layout of
            ChevronLeft ->
                [ -- vänster: chevron + titel i en rad
                  div [ A.class "min-w-0 inline-flex items-center gap-2" ]
                    [ chevronView
                    , span [ A.class titleCls, A.title row.title ] [ text row.title ]
                    ]
                , -- höger: extras (om några)
                  div [ A.class "inline-flex items-center gap-2" ] rightExtras
                ]

            ChevronRight ->
                [ -- vänster: titel
                  span [ A.class titleCls, A.title row.title ] [ text row.title ]
                , -- höger: extras + chevron
                  div [ A.class "inline-flex items-center gap-2" ]
                    (rightExtras ++ [ chevronView ])
                ]
        )



-- Compact view ------------------------------


featureRowViewDoneCompact : Model -> Feature -> List (Html Msg)
featureRowViewDoneCompact model row =
    let
        featureCellCompact : Feature -> Html Msg
        featureCellCompact r =
            div
                [ A.class "px-2 py-1 rounded-xl bg-emerald-50/50 border border-emerald-300 text-emerald-950 sticky left-0 z-10"
                , A.style "backdrop-filter" "blur(2px)"
                ]
                [ featureHeader ChevronLeft DoneCompact r [] ]

        cells : List (Html Msg)
        cells =
            List.map (\ix -> compactSprintCell model row ix)
                (List.range 1 model.sprintCount)
    in
    featureCellCompact row :: cells


compactSprintCell : Model -> Feature -> Int -> Html Msg
compactSprintCell model row ix =
    let
        toneCls =
            rowTintClass model row

        baseCls =
            "px-2 py-1 rounded-lg border text-[12px] flex items-center justify-center" ++ toneCls

        isDeliveryHere =
            featureDeliverySprint row == Just ix
    in
    div [ A.class baseCls ]
        (if isDeliveryHere then
            [ div [ A.class "inline-flex items-center gap-2" ]
                [ statusPill Tiny row.status
                , warnBadge Tiny (selectWarnKind row)
                ]
            ]

         else
            [ text "" ]
        )



-- Normal view -----------------------------------
-- FIX: Kanske borde vi fixa så vi endast har denna...och inte compact??


featureRowView : Model -> Feature -> List (Html Msg)
featureRowView model row =
    let
        mode =
            rowMode model row

        can =
            canInteract mode

        warnKind : FeatureWarn
        warnKind =
            selectWarnKind row

        -- INSIDE featureRowView, in the local `featureCell`:
        featureCell : Html Msg
        featureCell =
            let
                baseCls : String
                baseCls =
                    "px-3 py-2 rounded-xl border font-medium sticky left-0 z-10 transition "
                        ++ rowTintClass model row
                        ++ " border-slate-200"

                -- “Unscheduled” tray (Missing / WholePI / OutsidePI stories)
                tray : List (Html Msg)
                tray =
                    let
                        unsched =
                            unscheduledStories row
                    in
                    if List.isEmpty unsched then
                        []

                    else
                        [ div [ A.class "mt-1 rounded-lg border border-slate-200 bg-white/70 p-1" ]
                            [ div [ A.class "text-[10px] uppercase tracking-wide text-slate-500 mb-1" ] [ text "Unscheduled" ]
                            , div [ A.class "flex flex-col gap-1" ]
                                (List.map (\s -> storyCard False can s) unsched)
                            ]
                        ]

                -- When delivery is Nothing, also show the feature card here (draggable only if can)
                maybeFeatureCard : List (Html Msg)
                maybeFeatureCard =
                    case row.iteration of
                        InPI _ ->
                            []

                        _ ->
                            [ featureCard False warnKind can row ]
            in
            div
                [ A.class baseCls
                , A.style "backdrop-filter" "blur(2px)"
                ]
                (featureHeader ChevronLeft mode row [] :: maybeFeatureCard ++ tray)

        cells : List (Html Msg)
        cells =
            List.map (\ix -> sprintCell model row ix)
                (List.range 1 model.sprintCount)
    in
    featureCell :: cells


sprintCell : Model -> Feature -> Int -> Html Msg
sprintCell model row ix =
    let
        -- 0) Row mode & interactivity -------------------------------
        mode : RowMode
        mode =
            rowMode model row

        can : Bool
        can =
            canInteract mode

        -- 1) Content in this cell -----------------------------------
        storiesHere : List Story
        storiesHere =
            row.stories
                |> List.filter
                    (\s ->
                        case s.iteration of
                            InPI k ->
                                k == ix

                            _ ->
                                False
                    )

        isDeliveryHere : Bool
        isDeliveryHere =
            case row.iteration of
                InPI k ->
                    k == ix

                _ ->
                    False

        -- 2) Drag target validity (valid row)
        validStoryTarget : Bool
        validStoryTarget =
            case model.draggingStory of
                Nothing ->
                    False

                Just sid ->
                    let
                        belongsHere =
                            List.any (\s -> s.id == sid) row.stories

                        withinDeliveryGate =
                            case row.iteration of
                                InPI d ->
                                    ix <= d

                                _ ->
                                    True
                    in
                    belongsHere && withinDeliveryGate

        validDeliveryTarget : Bool
        validDeliveryTarget =
            case model.draggingDelivery of
                Nothing ->
                    False

                Just fid ->
                    -- Only this feature's delivery, and only at/after last story sprint
                    if fid /= row.featureId then
                        False

                    else
                        ix >= lastStorySprint row

        -- 3) Hover state (valid sprint && valid row)
        hoveredForStory : Bool
        hoveredForStory =
            model.hoverStorySprint == Just ix && validStoryTarget

        hoveredForDelivery : Bool
        hoveredForDelivery =
            model.hoverDeliverySprint == Just ix && validDeliveryTarget

        -- 4) Visuals -------------------------------------------------
        toneCls : String
        toneCls =
            rowTintClass model row

        -- subtle base, plus “valid target” and “hovered” accents
        baseClass : String
        baseClass =
            "min-w-0 px-3 py-2 rounded-xl border min-h-[84px] flex flex-col gap-1 transition " ++ toneCls

        validCls : String
        validCls =
            if can && (validStoryTarget || validDeliveryTarget) then
                -- light border change + dashed hint = “this cell can accept drop”
                " border-indigo-300 border-dashed "

            else
                " border-slate-200 "

        hoverCls : String
        hoverCls =
            if can && (hoveredForStory || hoveredForDelivery) then
                -- on actual hover, make it obviously active
                " ring-2 ring-indigo-400 bg-indigo-50/40 "

            else
                ""

        forbidClass : String
        forbidClass =
            if can then
                ""

            else
                " cursor-not-allowed"

        -- 5) Handlers (built from validity; attached by the ROW gate)
        storyHandlers : List (Html.Attribute Msg)
        storyHandlers =
            if validStoryTarget then
                [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverStory (Just ix)))
                , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverStory (Just ix), True ))
                , Html.Events.on "dragleave" (Json.Decode.succeed (HoverStory Nothing))
                , Html.Events.on "drop" (Json.Decode.succeed (StoryDrop ix))
                ]

            else
                []

        deliveryHandlers : List (Html.Attribute Msg)
        deliveryHandlers =
            if validDeliveryTarget then
                [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverDelivery (Just ix)))
                , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverDelivery (Just ix), True ))
                , Html.Events.on "dragleave" (Json.Decode.succeed (HoverDelivery Nothing))
                , Html.Events.on "drop" (Json.Decode.succeed (DeliveryDrop ix))
                ]

            else
                []

        -- 6) Ghosting ------------------------------------------------
        -- Ghost the original story chip when that specific story is being dragged
        isGhostStory : Story -> Bool
        isGhostStory s =
            case model.draggingStory of
                Just sid ->
                    sid
                        == s.id
                        && (case s.iteration of
                                InPI k ->
                                    k == ix

                                _ ->
                                    False
                           )

                Nothing ->
                    False

        -- Ghost the original feature delivery card in its original cell
        isGhostDelivery : Bool
        isGhostDelivery =
            case ( model.draggingDelivery, row.iteration ) of
                ( Just fid, InPI k ) ->
                    fid == row.featureId && k == ix

                _ ->
                    False

        -- 7) Content helpers -----------------------------------------
        emptyHint : List (Html Msg)
        emptyHint =
            if List.isEmpty storiesHere && not isDeliveryHere then
                [ div [ A.class "text-[11px] text-slate-400 italic" ] [ text "–" ] ]

            else
                []
    in
    div
        (A.class (baseClass ++ validCls ++ hoverCls ++ forbidClass)
            :: attrsIf can (storyHandlers ++ deliveryHandlers)
        )
        (List.map (\s -> storyCard (isGhostStory s) can s) storiesHere
            ++ (if isDeliveryHere then
                    [ featureCard isGhostDelivery (selectWarnKind row) can row ]

                else
                    []
               )
            ++ emptyHint
        )


storyCard : Bool -> Bool -> Story -> Html Msg
storyCard isGhost can s =
    let
        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        dragAttrs : List (Html.Attribute Msg)
        dragAttrs =
            if can then
                [ A.attribute "draggable" "true"
                , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
                , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
                , Html.Events.on "dragstart" (Json.Decode.succeed (StoryDragStart s.id))
                , Html.Events.on "dragend" (Json.Decode.succeed StoryDragEnd)
                ]

            else
                []

        disabledCls =
            if can then
                ""

            else
                " opacity-60 cursor-not-allowed"
    in
    div
        ([ A.class
            ("min-w-0 flex items-center justify-between gap-2 pl-1 pr-2 py-1 rounded-lg border select-none "
                ++ cardToneForStatus s.status
                ++ ghostClass
                ++ disabledCls
            )
         , A.title s.title
         ]
            ++ dragAttrs
        )
        [ -- LEFT: rail + title (truncates nicely)
          div [ A.class "min-w-0 inline-flex items-center gap-1.5" ]
            (if can then
                [ dotRailPx (railSpec Small Regular)
                , span [ A.class "font-medium truncate text-[13px]" ] [ text s.title ]
                ]

             else
                [ span [ A.class "font-medium truncate text-[13px]" ] [ text s.title ] ]
            )
        , -- RIGHT: warning badge (if any)
          div [ A.class "flex items-center shrink-0" ] [ storyIterationBadge Tiny s.iteration ]
        ]


featureCard : Bool -> FeatureWarn -> Bool -> Feature -> Html Msg
featureCard isGhost warnKind can row =
    let
        -- visuals
        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        -- borderColor =
        --     case warnKind of
        --         WarnAfter ->
        --             " border-amber-400"
        --
        --         WarnNeedsDelivery ->
        --             " border-amber-400"
        --
        --         WarnStoriesNotDone ->
        --             " border-amber-400"
        --
        --         NoWarn ->
        --             " border-indigo-300"
        dragAttrs : List (Html.Attribute Msg)
        dragAttrs =
            if can then
                [ A.attribute "draggable" "true"
                , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
                , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
                , Html.Events.on "dragstart" (Json.Decode.succeed (DeliveryDragStart row.featureId))
                , Html.Events.on "dragend" (Json.Decode.succeed DeliveryDragEnd)
                ]

            else
                []

        disabledCardClass =
            if can then
                ""

            else
                " opacity-60 cursor-not-allowed"

        chipsWrapperClass =
            if can then
                ""

            else
                "pointer-events-none opacity-80"
    in
    div
        (A.class
            ("mt-1 rounded-xl border select-none overflow-hidden "
                ++ cardToneForStatus row.status
                -- ++ borderColor
                ++ ghostClass
                ++ disabledCardClass
            )
            :: dragAttrs
        )
        [ -- ROW: rail + content
          div [ A.class "flex items-stretch" ]
            [ -- left rail (keep your existing rail call; you said you refactored it)
              if can then
                div [ A.class "ml-2 self-center" ]
                    [ dotRailPx (railSpec Medium Regular) ]

              else
                div [ A.class "ml-2" ] []
            , -- content
              div [ A.class "flex-1 pl-1 pr-2 py-2 min-w-0" ]
                [ -- row 1: title + status + warn
                  div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
                    [ span [ A.class "text-[13px] font-semibold truncate" ] [ text row.title ]
                    , div [ A.class "flex items-center gap-1 shrink-0" ]
                        [ warnBadge Tiny warnKind ]
                    ]
                , -- row 2: delivery + test chips
                  div [ A.class "mt-1 flex items-center justify-between gap-2 min-w-0" ]
                    [ -- statusPill Tiny row.status
                      testStrip can row.featureId row.tests
                    ]
                ]
            ]
        ]


testStrip : Bool -> Int -> Tests -> Html Msg
testStrip can featureId tests =
    let
        mk label active kind =
            testChipView Small
                { label = label, active = active }
                (if can then
                    [ Html.Events.onClick (ToggleTest featureId kind) ]

                 else
                    []
                )

        chips =
            [ mk "SIT" tests.sit SIT
            , mk "UAT" tests.uat UAT
            , mk "E2E" tests.e2e E2E
            ]
    in
    testStripView Tiny chips
