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
    , rows : List FeatureRow
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

        rowsFromAdo : List FeatureRow
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
                                { r | delivery = Just newSprintIx }

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


toggleTest : Int -> TestKind -> FeatureRow -> FeatureRow
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


lastStorySprint : FeatureRow -> Int
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


hasStoryAfterDelivery : FeatureRow -> Bool
hasStoryAfterDelivery row =
    case row.delivery of
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


selectWarnKind : FeatureRow -> FeatureWarn
selectWarnKind row =
    if row.status == Done then
        -- Feature marked Done; if any story is not Done -> warn
        if List.any (\s -> s.status /= Done) row.stories then
            WarnStoriesNotDone

        else
            NoWarn

    else
        -- Feature NOT done
        case row.delivery of
            Nothing ->
                WarnNeedsDelivery

            Just _ ->
                if hasStoryAfterDelivery row then
                    WarnAfter

                else
                    NoWarn



-- Is there ANY unfinished story on the row?


hasUnfinishedStories : FeatureRow -> Bool
hasUnfinishedStories row =
    row.stories |> List.any (\s -> s.status /= Done)



-- Is there ANY unfinished story specifically in sprint ix?


hasUnfinishedStoriesInSprint : FeatureRow -> Int -> Bool
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


unscheduledStories : FeatureRow -> List Story
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


rowMode : Model -> FeatureRow -> RowMode
rowMode model row =
    if row.status == Done then
        if Set.member row.featureId model.unlocked then
            DoneExpanded

        else
            DoneCompact

    else
        Active



-- Centralized interaction rule


canInteract : RowMode -> Bool
canInteract mode =
    case mode of
        Active ->
            True

        -- 🔒 Expanded-but-done rows are read-only
        DoneExpanded ->
            False

        -- compact rows are also read-only
        DoneCompact ->
            False


rowTintClass : Model -> FeatureRow -> String
rowTintClass model row =
    case rowMode model row of
        Active ->
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


rowView : Model -> FeatureRow -> List (Html Msg)
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


featureHeader : RowMode -> FeatureRow -> List (Html Msg) -> Html Msg
featureHeader mode row rightExtras =
    let
        -- smaller title in compact mode
        titleCls =
            case mode of
                DoneCompact ->
                    "text-[11px] font-semibold truncate"

                _ ->
                    "text-[12px] font-semibold truncate"

        -- chevron differs by mode
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

                Active ->
                    text ""
    in
    -- Title (left) · right-extras (middle) · Chevron (right)
    div [ A.class "flex items-center gap-2 min-w-0" ]
        [ span [ A.class titleCls, A.title row.title ] [ text row.title ]
        , div [ A.class "ml-auto inline-flex items-center gap-2" ] rightExtras
        , chevronView
        ]


statusPill : FeatureRow -> Html Msg
statusPill row =
    let
        ( statusLabel, statusCls ) =
            case row.status of
                Todo ->
                    ( "Todo", "bg-slate-100 border-slate-200 text-slate-700" )

                Doing ->
                    ( "Doing", "bg-sky-100 border-sky-200 text-sky-900" )

                Done ->
                    ( "Done", "bg-emerald-100 border-emerald-200 text-emerald-900" )
    in
    span
        [ A.class ("inline-flex items-center px-1.5 py-0.5 rounded-md border text-[10px] " ++ statusCls)
        , A.title ("Feature status: " ++ statusLabel)
        ]
        [ text statusLabel ]



-- Compact view ------------------------------
-- Small helpers (pure, composable)


maybeTestStrip : FeatureRow -> Maybe (Html Msg)
maybeTestStrip row =
    let
        dot : String -> Html Msg
        dot lbl =
            span
                [ A.class "inline-flex items-center justify-center px-1 py-0 rounded-md border border-indigo-300 bg-indigo-50 text-indigo-700 text-[9px] font-medium" ]
                [ text lbl ]

        chips =
            (if row.tests.sit then
                [ dot "SIT" ]

             else
                []
            )
                ++ (if row.tests.uat then
                        [ dot "UAT" ]

                    else
                        []
                   )
                ++ (if row.tests.e2e then
                        [ dot "E2E" ]

                    else
                        []
                   )
    in
    if List.isEmpty chips then
        Nothing

    else
        Just (div [ A.class "inline-flex items-center gap-1" ] chips)


maybeWarnChip : FeatureRow -> Maybe (Html Msg)
maybeWarnChip row =
    case selectWarnKind row of
        WarnNeedsDelivery ->
            Just <|
                span
                    [ A.class "inline-flex items-center gap-1 px-1 py-0 rounded-md border border-amber-300 bg-amber-50 text-amber-800 text-[9px]" ]
                    [ text "⚠ set delivery" ]

        WarnStoriesNotDone ->
            Just <|
                span
                    [ A.class "inline-flex items-center gap-1 px-1 py-0 rounded-md border border-rose-300 bg-rose-50 text-rose-800 text-[9px]" ]
                    [ text "⚠ stories" ]

        _ ->
            Nothing



-- Compact header right-side extras (status first, then optional bits)


compactRightExtras : FeatureRow -> Html Msg
compactRightExtras row =
    let
        pieces : List (Html Msg)
        pieces =
            [ maybeTestStrip row
            , maybeWarnChip row
            ]
                |> List.filterMap identity
    in
    div [ A.class "inline-flex items-center gap-2" ] (statusPill row :: pieces)


featureRowViewDoneCompact : Model -> FeatureRow -> List (Html Msg)
featureRowViewDoneCompact model row =
    let
        featureCellCompact : FeatureRow -> Html Msg
        featureCellCompact r =
            div
                [ A.class "px-2 py-1 rounded-xl bg-emerald-50/50 border border-emerald-300 text-emerald-950 sticky left-0 z-10"
                , A.style "backdrop-filter" "blur(2px)"
                ]
                [ featureHeader DoneCompact r [] ]

        -- [ featureHeader DoneCompact r [ compactRightExtras r ] ]
        cells : List (Html Msg)
        cells =
            List.map (\ix -> compactSprintCell model row ix)
                (List.range 1 model.sprintCount)
    in
    featureCellCompact row :: cells


compactSprintCell : Model -> FeatureRow -> Int -> Html Msg
compactSprintCell model row ix =
    let
        toneCls =
            rowTintClass model row

        baseCls =
            "px-2 py-1 rounded-lg border text-[12px] flex items-center justify-center" ++ toneCls

        -- hasUnfinishedHere =
        --     hasUnfinishedStoriesInSprint row ix
        isDeliveryHere =
            row.delivery == Just ix

        -- The delivery green dot; we emulate a “ring” by wrapping in a red bordered capsule when row has any unfinished.
        -- deliveryView : Html Msg
        -- deliveryView =
        --     if row.status == Done && isDeliveryHere then
        --         let
        --             hasAnyUnfinished =
        --                 hasUnfinishedStories row
        --
        --             wrapperCls =
        --                 if hasAnyUnfinished then
        --                     "inline-flex items-center justify-center rounded-full border border-rose-400 p-[1px]"
        --
        --                 else
        --                     "inline-flex items-center justify-center"
        --         in
        --         span [ A.class wrapperCls, A.title "Feature delivered here" ]
        --             [ span [ A.class "inline-block w-2.5 h-2.5 rounded-full bg-emerald-500" ] [] ]
        --
        --     else
        --         text ""
        -- unfinishedBadge : Html Msg
        -- unfinishedBadge =
        --     if hasUnfinishedHere then
        --         span
        --             [ A.class "inline-flex items-center justify-center w-4 h-4 rounded-full text-[10px] font-bold text-white bg-rose-500"
        --             , A.title "Unfinished story in this sprint"
        --             ]
        --             [ text "!" ]
        --
        --     else
        --         text ""
    in
    div [ A.class baseCls ]
        (if isDeliveryHere then
            [ compactRightExtras row ]
            -- [ deliveryView ]
            -- else if hasUnfinishedHere then
            --    [ unfinishedBadge ]

         else
            [ text "" ]
        )



-- Normal view -----------------------------------


featureRowView : Model -> FeatureRow -> List (Html Msg)
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
                    case row.delivery of
                        Nothing ->
                            [ featureCard False warnKind can row ]

                        _ ->
                            []
            in
            div
                [ A.class baseCls
                , A.style "backdrop-filter" "blur(2px)"
                ]
                (featureHeader mode row [] :: maybeFeatureCard ++ tray)

        cells : List (Html Msg)
        cells =
            List.map (\ix -> sprintCell model row ix)
                (List.range 1 model.sprintCount)
    in
    featureCell :: cells


sprintCell : Model -> FeatureRow -> Int -> Html Msg
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
            row.delivery == Just ix

        -- 2) Visuals -------------------------------------------------
        toneCls =
            rowTintClass model row

        baseClass : String
        baseClass =
            "min-w-0 px-3 py-2 rounded-xl border min-h-[84px] flex flex-col gap-1 transition " ++ toneCls

        -- colorClass : String
        -- colorClass =
        --     "bg-white border-slate-200 "
        emptyHint : List (Html Msg)
        emptyHint =
            if List.isEmpty storiesHere && not isDeliveryHere then
                [ div [ A.class "text-[11px] text-slate-400 italic" ] [ text "–" ] ]

            else
                []

        forbidClass : String
        forbidClass =
            if can then
                ""

            else
                " cursor-not-allowed"

        -- keep/extend your own “forbidden” styling if needed
        -- 3) Warn kind (after/delivery/stories-not-done) -------------
        warnKind : FeatureWarn
        warnKind =
            selectWarnKind row

        -- 4) Cell-level validity (active rows only) ------------------
        -- Story valid target: dragging a story that belongs to THIS row
        isValidTargetStory : Bool
        isValidTargetStory =
            case model.draggingStory of
                Nothing ->
                    False

                Just sid ->
                    row.stories
                        |> List.any (\s -> s.id == sid)

        -- Delivery valid target: dragging this row’s feature AND ix >= last story sprint
        isValidTargetDelivery : Bool
        isValidTargetDelivery =
            case model.draggingDelivery of
                Nothing ->
                    False

                Just fid ->
                    if fid /= row.featureId then
                        False

                    else
                        ix >= lastStorySprint row

        -- 5) Handlers (built from validity; attached by the ROW gate) -
        storyHandlers : List (Html.Attribute Msg)
        storyHandlers =
            if isValidTargetStory then
                [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverStory (Just ix)))
                , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverStory (Just ix), True ))
                , Html.Events.on "dragleave" (Json.Decode.succeed (HoverStory Nothing))
                , Html.Events.on "drop" (Json.Decode.succeed (StoryDrop ix))
                ]

            else
                []

        deliveryHandlers : List (Html.Attribute Msg)
        deliveryHandlers =
            if isValidTargetDelivery then
                [ Html.Events.on "dragenter" (Json.Decode.succeed (HoverDelivery (Just ix)))
                , Html.Events.preventDefaultOn "dragover" (Json.Decode.succeed ( HoverDelivery (Just ix), True ))
                , Html.Events.on "dragleave" (Json.Decode.succeed (HoverDelivery Nothing))
                , Html.Events.on "drop" (Json.Decode.succeed (DeliveryDrop ix))
                ]

            else
                []

        -- 6) Ghosting (keep your logic; placeholders here) -----------
        isGhostStory : Story -> Bool
        isGhostStory _ =
            False

        isGhostDelivery : Bool
        isGhostDelivery =
            False
    in
    div
        (A.class
            (baseClass ++ forbidClass)
            -- 👉 Row-level gate: attaches ALL handlers only when row is interactive
            :: attrsIf can (storyHandlers ++ deliveryHandlers)
        )
        (List.map (\s -> storyCard (isGhostStory s) can s) storiesHere
            ++ (if isDeliveryHere then
                    [ featureCard isGhostDelivery warnKind can row ]

                else
                    []
               )
            ++ emptyHint
        )


storyCard : Bool -> Bool -> Story -> Html Msg
storyCard isGhost can s =
    let
        ( bg, fg ) =
            case s.status of
                Todo ->
                    ( "bg-slate-100 border-slate-200", "text-slate-700" )

                Doing ->
                    ( "bg-sky-100 border-sky-200", "text-sky-900" )

                Done ->
                    ( "bg-emerald-100 border-emerald-200", "text-emerald-900" )

        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        badgeView : Html Msg
        badgeView =
            case s.iteration of
                Missing ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-2 py-1 rounded-lg border border-amber-300 bg-amber-50 text-amber-800 text-[11px] shrink-0"
                        , A.title "Story has no sprint set"
                        ]
                        [ text "⚠︎ no sprint" ]

                WholePI ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-2 py-1 rounded-lg border border-amber-300 bg-amber-50 text-amber-800 text-[11px] shrink-0"
                        , A.title "Story is set to whole PI"
                        ]
                        [ text "⚠︎ whole PI" ]

                OutsidePI ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-2 py-1 rounded-lg border border-rose-300 bg-rose-50 text-rose-800 text-[11px] shrink-0"
                        , A.title "Story belongs to another PI"
                        ]
                        [ text "⚠︎ wrong PI" ]

                InPI _ ->
                    text ""

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
        ([ A.class ("min-w-0 flex items-center justify-between gap-2 pl-1 pr-2 py-1 rounded-lg border select-none " ++ bg ++ " " ++ fg ++ ghostClass ++ disabledCls)
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
          div [ A.class "flex items-center shrink-0" ] [ badgeView ]
        ]


featureCard : Bool -> FeatureWarn -> Bool -> FeatureRow -> Html Msg
featureCard isGhost warnKind can row =
    let
        -- visuals
        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        borderColor =
            case warnKind of
                WarnAfter ->
                    " border-amber-400"

                WarnNeedsDelivery ->
                    " border-amber-400"

                WarnStoriesNotDone ->
                    " border-amber-400"

                NoWarn ->
                    " border-indigo-300"

        warnBadgeView : Html Msg
        warnBadgeView =
            case warnKind of
                WarnAfter ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-1.5 py-0.5 rounded-md border border-amber-300 bg-amber-50 text-amber-800 text-[10px] shrink-0"
                        , A.title "One or more stories are scheduled after delivery"
                        ]
                        [ text "⚠︎ after" ]

                WarnNeedsDelivery ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-1.5 py-0.5 rounded-md border border-amber-300 bg-amber-50 text-amber-800 text-[10px] shrink-0"
                        , A.title "Feature has no delivery sprint set"
                        ]
                        [ text "⚠︎ set delivery" ]

                WarnStoriesNotDone ->
                    span
                        [ A.class "inline-flex items-center gap-1 px-1.5 py-0.5 rounded-md border border-amber-300 bg-amber-50 text-amber-800 text-[10px] shrink-0"
                        , A.title "Feature is Done but some stories are not closed"
                        ]
                        [ text "⚠︎ stories" ]

                NoWarn ->
                    text ""

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
            ("mt-1 rounded-xl border bg-indigo-50/70 text-indigo-900 select-none overflow-hidden "
                ++ borderColor
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
                        [ warnBadgeView ]
                    ]
                , -- row 2: delivery + test chips
                  div [ A.class "mt-1 flex items-center justify-between gap-2 min-w-0" ]
                    [ statusPill row -- span [ A.class "text-[12px] text-indigo-900/80 font-medium" ] [ text "Delivery" ]
                    , div [ A.class ("flex items-center gap-1 shrink-0" ++ chipsWrapperClass) ]
                        (testChip can row.featureId SIT row.tests.sit
                            ++ testChip can row.featureId UAT row.tests.uat
                            ++ testChip can row.featureId E2E row.tests.e2e
                        )
                    ]
                ]
            ]
        ]


testChip : Bool -> Int -> TestKind -> Bool -> List (Html Msg)
testChip can featureId testKind isActive =
    let
        label =
            case testKind of
                SIT ->
                    "SIT"

                UAT ->
                    "UAT"

                E2E ->
                    "E2E"

        activeBase =
            "bg-indigo-100 border-indigo-300 text-indigo-700 font-semibold"

        activeHover =
            "hover:bg-indigo-200 hover:border-indigo-400"

        inactiveBase =
            "bg-white border-slate-200 text-slate-400"

        inactiveHover =
            "hover:bg-indigo-50 hover:border-indigo-200 hover:text-indigo-600"

        cls =
            if isActive then
                activeBase ++ " " ++ activeHover

            else
                inactiveBase ++ " " ++ inactiveHover
    in
    [ span
        (A.class ("inline-flex items-center justify-center px-[3px] py-0 rounded-full border text-[8px] cursor-pointer select-none transition " ++ cls)
            :: attrsIf can
                [ Html.Events.onClick (ToggleTest featureId testKind) ]
        )
        [ text label ]
    ]
