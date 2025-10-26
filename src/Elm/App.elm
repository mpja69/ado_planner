module App exposing (main)

import Browser
import Data.Ado as Ado
import Data.Translate as T
import Dict exposing (Dict)
import Html exposing (Html, button, div, span, text)
import Html.Attributes as A
import Html.Events
import Json.Decode
import Set exposing (Set)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Time exposing (Posix)
import Types exposing (..)



-- TODO:
-- Add status to features
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

        DoneExpanded ->
            True

        -- allow interaction when user explicitly unlocked
        DoneCompact ->
            False


isMismatchClosedFeature : FeatureRow -> Bool
isMismatchClosedFeature row =
    row.status
        == Done
        && List.any (\s -> s.status /= Done) row.stories


closedSprintIx : Model -> FeatureRow -> Maybe Int
closedSprintIx model row =
    case row.closedDate of
        Nothing ->
            Nothing

        Just posix ->
            dateToSprint model posix



-- Stub: fill in using your PI boundaries/cadence later


dateToSprint : Model -> Posix -> Maybe Int
dateToSprint _ _ =
    Nothing



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


featureHeader : RowMode -> FeatureRow -> Html Msg
featureHeader mode row =
    let
        -- title size differs a bit in compact vs expanded
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
    -- inner header row: title left, chevron right
    div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
        [ span [ A.class titleCls, A.title row.title ] [ text row.title ]
        , chevronView
        ]



-- Compact view ------------------------------


featureRowViewDoneCompact : Model -> FeatureRow -> List (Html Msg)
featureRowViewDoneCompact model row =
    let
        leftCell : Html Msg
        leftCell =
            div
                [ A.class "sticky left-0 z-10" ]
                [ -- compact wrapper tone stays here
                  div
                    [ A.class "px-2 py-1 rounded-xl bg-emerald-50/50 border border-emerald-300 text-emerald-950" ]
                    [ featureHeader DoneCompact row ]
                ]

        sprintCells : List (Html Msg)
        sprintCells =
            List.map (\ix -> compactSprintCell model row ix)
                (List.range 1 model.sprintCount)
    in
    leftCell :: sprintCells


compactSprintCell : Model -> FeatureRow -> Int -> Html Msg
compactSprintCell model row ix =
    let
        isDeliveryHere =
            row.delivery == Just ix

        closedIx : Maybe Int
        closedIx =
            closedSprintIx model row

        mismatchHere : Bool
        mismatchHere =
            case ( row.delivery, closedIx ) of
                ( Just d, Just c ) ->
                    d /= c && d == ix

                _ ->
                    False

        markers : List (Html Msg)
        markers =
            (if row.status == Done && isDeliveryHere then
                [ doneHereDotCompact ]

             else
                []
            )
                ++ (if mismatchHere then
                        [ warnMismatchDotCompact ]

                    else
                        []
                   )
    in
    div
        [ A.class "px-2 py-1 rounded-lg border border-slate-200 bg-white min-h-[28px] flex items-center justify-start gap-1" ]
        (if List.isEmpty markers then
            [ span [ A.class "text-[10px] text-slate-300" ] [ text "–" ] ]

         else
            markers
        )


doneHereDotCompact : Html Msg
doneHereDotCompact =
    span
        [ A.class "inline-block w-2 h-2 rounded-full bg-emerald-500"
        , A.title "Delivered here"
        ]
        []


warnMismatchDotCompact : Html Msg
warnMismatchDotCompact =
    span
        [ A.class "inline-flex items-center justify-center w-4 h-4 rounded-full border border-amber-300 bg-amber-50 text-amber-700 text-[10px] leading-none"
        , A.title "Closed date does not match Iteration Path"
        ]
        [ text "!" ]



-- Normal view -----------------------------------


featureRowView : Model -> FeatureRow -> List (Html Msg)
featureRowView model row =
    let
        mode =
            rowMode model row

        featureCell : Html Msg
        featureCell =
            let
                baseCls =
                    "px-3 py-2 rounded-xl bg-slate-100/70 border border-slate-200 font-medium sticky left-0 z-10 transition"

                toneCls =
                    case mode of
                        DoneExpanded ->
                            " bg-emerald-50/60 border-emerald-200"

                        _ ->
                            ""
            in
            div
                [ A.class (baseCls ++ toneCls)
                , A.style "backdrop-filter" "blur(2px)"
                ]
                [ featureHeader mode row ]

        cells : List (Html Msg)
        cells =
            List.map (\ix -> sprintCell model row ix) (List.range 1 model.sprintCount)
    in
    featureCell :: cells


sprintCell : Model -> FeatureRow -> Int -> Html Msg
sprintCell model row ix =
    let
        storiesHere =
            List.filter
                (\s ->
                    case s.iteration of
                        InPI j ->
                            j == ix

                        _ ->
                            False
                )
                row.stories

        isDeliveryHere =
            row.delivery == Just ix

        minDeliveryIx =
            lastStorySprint row

        -- Delivery-drag state (ingen global dimning)
        isDraggingThisRowDelivery =
            model.draggingDelivery == Just row.featureId

        isValidTargetDelivery =
            isDraggingThisRowDelivery && ix >= minDeliveryIx

        isHoverDeliveryHere =
            isValidTargetDelivery && model.hoverDeliverySprint == Just ix

        -- Story-drag state
        isDraggingThisRowStory =
            case model.draggingStory of
                Just sid ->
                    List.any (\s -> s.id == sid) row.stories

                Nothing ->
                    False

        allowedByDeliveryForStory =
            case row.delivery of
                Just d ->
                    ix <= d

                Nothing ->
                    True

        isValidTargetStory =
            isDraggingThisRowStory && allowedByDeliveryForStory

        isHoverStoryHere =
            isValidTargetStory && model.hoverStorySprint == Just ix

        baseClass =
            "min-w-0 px-3 py-2 rounded-xl border min-h-[84px] flex flex-col gap-1 transition"

        colorClass =
            if isHoverDeliveryHere then
                " bg-blue-50 border-blue-300 shadow-inner"

            else if isHoverStoryHere then
                " bg-sky-50 border-sky-300 shadow-inner"

            else
                " bg-white border-slate-200"

        forbidClass =
            if isDraggingThisRowStory && not allowedByDeliveryForStory then
                " opacity-40 saturate-50 cursor-not-allowed"

            else
                ""

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

        emptyHint =
            if List.isEmpty storiesHere && not isDeliveryHere then
                [ div [ A.class "text-[11px] text-slate-400 italic" ] [ text "–" ] ]

            else
                []

        isGhostStory : Story -> Bool
        isGhostStory s =
            model.draggingStory == Just s.id

        isGhostDelivery : Bool
        isGhostDelivery =
            model.draggingDelivery == Just row.featureId
    in
    div (A.class (baseClass ++ colorClass ++ forbidClass) :: (storyHandlers ++ deliveryHandlers))
        (List.map (\s -> storyCard (isGhostStory s) s) storiesHere
            ++ (if isDeliveryHere then
                    [ featureCard isGhostDelivery
                        (if hasStoryAfterDelivery row then
                            WarnAfter

                         else
                            NoWarn
                        )
                        row
                    ]

                else
                    []
               )
            ++ emptyHint
        )


storyCard : Bool -> Story -> Html Msg
storyCard isGhost s =
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

        -- compute badge (if any), but do not place it inline with the title;
        -- we'll render it in a right-aligned container
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
    in
    -- outer: a single row with left group (rail+title) and right group (badge)
    div
        [ A.class ("min-w-0 flex items-center justify-between gap-2 pl-1 pr-2 py-1 rounded-lg border select-none " ++ bg ++ " " ++ fg ++ ghostClass)
        , A.title s.title
        , A.attribute "draggable" "true"
        , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
        , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
        , Html.Events.on "dragstart" (Json.Decode.succeed (StoryDragStart s.id))
        , Html.Events.on "dragend" (Json.Decode.succeed StoryDragEnd)
        ]
        [ -- LEFT GROUP: rail + title (kept together, truncates nicely)
          div [ A.class "min-w-0 inline-flex items-center gap-1.5" ]
            [ dotRailPx
                { widthPx = 15
                , heightPx = 20
                , padLeftPx = 2
                , padTopPx = 5 -- centers 3 rows in 20px
                , cols = 2
                , rows = 3 -- story: 3 dots high
                , dotDiameterPx = 2
                , gapXPx = 4
                , gapYPx = 2
                , dotColor = "rgba(99,102,241,0.50)"
                , bgColor = "transparent"
                , rounded = True
                , mode = Regular
                }
            , span [ A.class "font-medium truncate text-[13px]" ] [ text s.title ]
            ]
        , -- RIGHT GROUP: warning badge (if empty, takes no space)
          div [ A.class "flex items-center shrink-0" ] [ badgeView ]
        ]


featureCard : Bool -> FeatureWarn -> FeatureRow -> Html Msg
featureCard isGhost warnKind row =
    let
        -- 1) Visuals: ghosting + borders
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

                NoWarn ->
                    " border-indigo-300"

        -- 2) Status pill (Todo/Doing/Done)
        ( statusLabel, statusCls ) =
            case row.status of
                Todo ->
                    ( "Todo", "bg-slate-100 border-slate-200 text-slate-700" )

                Doing ->
                    ( "Doing", "bg-sky-100 border-sky-200 text-sky-900" )

                Done ->
                    ( "Done", "bg-emerald-100 border-emerald-200 text-emerald-900" )

        statusPill : Html Msg
        statusPill =
            span
                [ A.class ("inline-flex items-center px-1.5 py-0.5 rounded-md border text-[10px] " ++ statusCls)
                , A.title ("Feature status: " ++ statusLabel)
                ]
                [ text statusLabel ]

        -- 3) Warning badge
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

                NoWarn ->
                    text ""

        -- 4) Interactions disabled when Done (#4)
        isDisabled : Bool
        isDisabled =
            row.status == Done

        dragAttrs : List (Html.Attribute Msg)
        dragAttrs =
            if isDisabled then
                []

            else
                [ A.attribute "draggable" "true"
                , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
                , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
                , Html.Events.on "dragstart" (Json.Decode.succeed (DeliveryDragStart row.featureId))
                , Html.Events.on "dragend" (Json.Decode.succeed DeliveryDragEnd)
                ]

        disabledCardClass =
            if isDisabled then
                " opacity-60 cursor-not-allowed"

            else
                ""

        chipsWrapperClass =
            if isDisabled then
                "pointer-events-none opacity-80"

            else
                ""
    in
    div
        ([ A.class ("mt-1 rounded-xl border bg-indigo-50/70 text-indigo-900 select-none overflow-hidden " ++ borderColor ++ ghostClass ++ disabledCardClass) ]
            ++ dragAttrs
        )
        [ -- ROW: rail + content as siblings
          div [ A.class "flex items-stretch" ]
            [ -- left rail (visual handle)
              div [ A.class "ml-2 self-center" ]
                [ dotRailPx
                    { widthPx = 15
                    , heightPx = 20
                    , padLeftPx = 2
                    , padTopPx = 1
                    , cols = 2
                    , rows = 5
                    , dotDiameterPx = 2
                    , gapXPx = 4
                    , gapYPx = 2
                    , dotColor = "rgba(99,102,241,0.50)"
                    , bgColor = "transparent"
                    , rounded = True
                    , mode = Regular
                    }
                ]
            , -- content
              div [ A.class "flex-1 pl-1 pr-2 py-2 min-w-0" ]
                [ -- row 1: title + status + warning badge
                  div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
                    [ span [ A.class "text-[13px] font-semibold truncate" ] [ text row.title ]
                    , div [ A.class "flex items-center gap-1 shrink-0" ]
                        [ statusPill, warnBadgeView ]
                    ]

                -- row 2: Delivery + test chips (chips disabled when Done)
                , div [ A.class "mt-1 flex items-center justify-between gap-2 min-w-0" ]
                    [ span [ A.class "text-[12px] text-indigo-900/80 font-medium" ] [ text "Delivery" ]
                    , div [ A.class ("flex items-center gap-1 shrink-0 " ++ chipsWrapperClass) ]
                        (testChip row.featureId SIT row.tests.sit
                            ++ testChip row.featureId UAT row.tests.uat
                            ++ testChip row.featureId E2E row.tests.e2e
                        )
                    ]
                ]
            ]
        ]


testChip : Int -> TestKind -> Bool -> List (Html Msg)
testChip featureId testKind isActive =
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
        [ A.class ("inline-flex items-center justify-center px-[3px] py-0 rounded-full border text-[8px] cursor-pointer select-none transition " ++ cls)
        , Html.Events.onClick (ToggleTest featureId testKind)
        ]
        [ text label ]
    ]


type PatternMode
    = Regular
    | Staggered


type alias RailSpecPx =
    { widthPx : Int
    , heightPx : Int
    , padLeftPx : Int
    , padTopPx : Int
    , cols : Int
    , rows : Int
    , dotDiameterPx : Int
    , gapXPx : Int
    , gapYPx : Int
    , dotColor : String
    , bgColor : String
    , rounded : Bool
    , mode : PatternMode
    }



-- Absolute-pixel rail: 1 SVG unit == 1 CSS pixel. No scaling.


dotRailPx : RailSpecPx -> Html msg
dotRailPx p =
    let
        w =
            max 1 p.widthPx

        h =
            max 1 p.heightPx

        pl =
            max 0 p.padLeftPx

        pt =
            max 0 p.padTopPx

        cols =
            max 1 p.cols

        rows =
            max 1 p.rows

        d =
            max 1 p.dotDiameterPx

        r =
            d // 2

        gx =
            max 0 p.gapXPx

        gy =
            max 0 p.gapYPx

        cellW =
            d + gx

        cellH =
            d + gy

        startX =
            pl + r

        startY =
            pt + r

        range n =
            List.range 0 (n - 1)

        circlesRegular : List (Svg msg)
        circlesRegular =
            range cols
                |> List.concatMap
                    (\c ->
                        let
                            cx =
                                startX + c * cellW
                        in
                        range rows
                            |> List.map
                                (\rIdx ->
                                    let
                                        cy =
                                            startY + rIdx * cellH
                                    in
                                    Svg.circle
                                        [ SA.cx (String.fromInt cx)
                                        , SA.cy (String.fromInt cy)
                                        , SA.r (String.fromInt r)
                                        , SA.fill p.dotColor
                                        ]
                                        []
                                )
                    )

        circlesStaggered : List (Svg msg)
        circlesStaggered =
            range cols
                |> List.concatMap
                    (\c ->
                        let
                            cx =
                                startX + c * cellW

                            isOdd =
                                modBy 2 c == 1

                            rowsHere =
                                if isOdd then
                                    max 0 (rows - 1)

                                else
                                    rows

                            shiftY =
                                if isOdd then
                                    cellH // 2

                                else
                                    0
                        in
                        range rowsHere
                            |> List.map
                                (\rIdx ->
                                    let
                                        cy =
                                            startY + shiftY + rIdx * cellH
                                    in
                                    Svg.circle
                                        [ SA.cx (String.fromInt cx)
                                        , SA.cy (String.fromInt cy)
                                        , SA.r (String.fromInt r)
                                        , SA.fill p.dotColor
                                        ]
                                        []
                                )
                    )

        circles_ =
            case p.mode of
                Regular ->
                    circlesRegular

                Staggered ->
                    circlesStaggered

        roundedCls =
            if p.rounded then
                " rounded-l-xl"

            else
                ""
    in
    -- Outer container: fixed px, no scaling
    div
        [ A.class ("relative shrink-0" ++ roundedCls)
        , A.style "width" (String.fromInt w ++ "px")
        , A.style "height" (String.fromInt h ++ "px")
        , A.style "backgroundColor" p.bgColor
        , A.attribute "aria-hidden" "true"
        ]
        [ svg
            [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
            , SA.width (String.fromInt w)
            , SA.height (String.fromInt h)
            , SA.shapeRendering "geometricPrecision"
            ]
            circles_
        ]
