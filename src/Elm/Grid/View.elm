module Grid.View exposing (Toggles, appBgColor, view)

import Grid.Logic as GL
import Grid.Msg as GM
import Grid.Types as GT
import Html exposing (Html, button, div, span, text)
import Html.Attributes as A
import Html.Events as E
import Json.Decode
import Svg.Attributes
import Types exposing (..)
import Ui.Icons as Icons
import Ui.Rails exposing (..)
import Ui.Theme exposing (..)


type alias Toggles =
    { showTests : Bool
    , editableTests : Bool
    }



-- HELPERS


attrsIf : Bool -> List (Html.Attribute GM.Msg) -> List (Html.Attribute GM.Msg)
attrsIf cond attrs =
    if cond then
        attrs

    else
        []


appBgColor : String
appBgColor =
    " bg-white"


gridBgColor : String
gridBgColor =
    "  bg-slate-50/70 rounded-xl p-2"


cellBgColor : String
cellBgColor =
    "  bg-white/95 backdrop-blur"



-- Used with toggles


maybe : Bool -> Html msg -> List (Html msg)
maybe cond node =
    if cond then
        [ node ]

    else
        []



-- VIEW


view : Toggles -> GT.Model -> Html GM.Msg
view toggles model =
    let
        hasUnscheduled : Bool
        hasUnscheduled =
            GL.hasUnscheduledItems model.rows

        showTray : Bool
        showTray =
            hasUnscheduled && model.showUnscheduled

        leftColWidth : String
        leftColWidth =
            if showTray then
                -- fullt läge: plats för feature + stories
                "220px"

            else if hasUnscheduled then
                -- det finns oschemalagda items, men användaren har vikt ihop kolumnen:
                -- gör den bara tillräckligt bred för chevronen
                "28px"

            else
                -- inga unscheduled alls → i praktiken nästan ingen första kolumn
                "12px"

        templateCols =
            leftColWidth ++ " repeat(" ++ String.fromInt model.sprintCount ++ ", minmax(0, 1fr))"
    in
    div
        [ A.style "display" "grid"
        , A.style "grid-template-columns" templateCols
        , A.class ("gap-2" ++ gridBgColor)
        ]
        (headerRow hasUnscheduled showTray model.sprintCount
            :: List.concatMap (featureRowView toggles model showTray) model.rows
        )



-- headerRow : Bool -> Bool -> Int -> Html GM.Msg
-- headerRow hasUnscheduled showTray n =
--     let
--         headCell s =
--             div [ A.class "text-xs font-semibold uppercase tracking-wide text-slate-600 px-2 py-1" ] [ text s ]
--
--         mutedCell =
--             div [ A.class "px-0 py-0" ] []
--
--         unscheduledHeader : Html GM.Msg
--         unscheduledHeader =
--             if not hasUnscheduled then
--                 mutedCell
--
--             else
--                 div
--                     [ A.class "flex items-center gap-2 px-2 py-1 text-xs font-semibold uppercase tracking-wide text-slate-600" ]
--                     [ text "Not in a Sprint in this PI"
--                     , button
--                         [ A.type_ "button"
--                         , A.class "inline-flex items-center justify-center w-5 h-5 rounded hover:bg-slate-200"
--                         , E.onClick GM.ToggleUnscheduled
--                         ]
--                         [ Icons.chevronDown
--                             [ Svg.Attributes.class
--                                 ("w-3 h-3 transition-transform "
--                                     ++ (if showTray then
--                                             "rotate-0"
--
--                                         else
--                                             "-rotate-90"
--                                        )
--                                 )
--                             ]
--                         ]
--                     ]
--     in
--     div [ A.class "contents" ]
--         (unscheduledHeader
--             :: List.map (\i -> headCell ("Sprint " ++ String.fromInt i)) (List.range 1 n)
--         )


headerRow : Bool -> Bool -> Int -> Html GM.Msg
headerRow hasUnscheduled showTray n =
    let
        headCell s =
            div
                [ A.class "text-xs font-semibold uppercase tracking-wide text-slate-600 px-2 py-1" ]
                [ text s ]

        mutedCell =
            div [ A.class "px-0 py-0" ] []

        chevronButton : Html GM.Msg
        chevronButton =
            button
                [ A.type_ "button"
                , A.class "inline-flex items-center justify-center w-5 h-5 rounded hover:bg-slate-200"
                , E.onClick GM.ToggleUnscheduled
                ]
                [ Icons.chevronDown
                    [ Svg.Attributes.class
                        ("w-3 h-3 transition-transform "
                            ++ (if showTray then
                                    "rotate-0"

                                else
                                    "-rotate-90"
                               )
                        )
                    ]
                ]

        unscheduledHeader : Html GM.Msg
        unscheduledHeader =
            if not hasUnscheduled then
                mutedCell

            else if showTray then
                -- öppet läge: text + chevron
                div
                    [ A.class "flex items-center gap-2 px-2 py-1 text-xs font-semibold uppercase tracking-wide text-slate-600" ]
                    [ text "Not in a Sprint in this PI"
                    , chevronButton
                    ]

            else
                -- hopfällt läge: bara en smal cell med chevronen
                div
                    [ A.class "flex items-center justify-center px-1 py-1" ]
                    [ chevronButton ]
    in
    div [ A.class "contents" ]
        (unscheduledHeader
            :: List.map
                (\i -> headCell ("Sprint " ++ String.fromInt i))
                (List.range 1 n)
        )


featureRowView : Toggles -> GT.Model -> Bool -> Feature -> List (Html GM.Msg)
featureRowView toggles model showTray row =
    let
        can =
            -- GL.canInteract row -- HACK: Always make the cards interactive!
            True

        --mode
        warnKind : FeatureWarn
        warnKind =
            GL.selectWarnKind row

        isUnscheduledFeature =
            GL.isUnscheduled row.iteration

        isUnscheduledStories =
            not (List.isEmpty (GL.unscheduledStories row))

        isUnscheduledItems =
            isUnscheduledFeature || isUnscheduledStories

        emptyFeatureCell : Html GM.Msg
        emptyFeatureCell =
            div [ A.class "p-0 m-0" ] []

        -- INSIDE featureRowView, in the local `featureCell`:
        fullFeatureCell : Html GM.Msg
        fullFeatureCell =
            let
                baseCls : String
                baseCls =
                    "px-3 py-2 rounded-xl border font-medium sticky left-0 z-10 transition "
                        ++ " border-slate-200"
                        ++ cellBgColor

                -- “Unscheduled” tray (Missing / WholePI / OutsidePI stories)
                tray : List (Html GM.Msg)
                tray =
                    if isUnscheduledStories then
                        [ div [ A.class "mt-1 rounded-lg border border-slate-200 bg-white/70 p-1" ]
                            [ div [ A.class "text-[9px] text-slate-500 uppercase" ] [ text "Stories" ]
                            , div [ A.class "flex flex-col gap-1" ]
                                (List.map (\s -> storyCard False can s) (GL.unscheduledStories row))
                            ]
                        ]

                    else
                        []

                maybeFeatureCard : List (Html GM.Msg)
                maybeFeatureCard =
                    if isUnscheduledFeature then
                        [ featureCard toggles False warnKind can row ]

                    else
                        []

                maybeText : List (Html GM.Msg)
                maybeText =
                    if isUnscheduledFeature then
                        [ span [ A.class "text-[9px] text-slate-500 uppercase" ] [ text "Feature" ] ]

                    else
                        []
            in
            div
                [ A.class baseCls
                , A.style "backdrop-filter" "blur(2px)"
                ]
                (div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
                    maybeText
                    :: maybeFeatureCard
                    ++ tray
                )

        cells : List (Html GM.Msg)
        cells =
            List.map (\ix -> sprintCell toggles model row ix)
                (List.range 1 model.sprintCount)
    in
    (if isUnscheduledItems && showTray then
        fullFeatureCell

     else
        emptyFeatureCell
    )
        :: cells


sprintCell : Toggles -> GT.Model -> Feature -> Int -> Html GM.Msg
sprintCell toggles model row ix =
    let
        can : Bool
        can =
            -- GL.canInteract row -- HACK: Always make the cards interactive!
            True

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
                        ix >= GL.lastStorySprint row

        -- 3) Hover state (valid sprint && valid row)
        hoveredForStory : Bool
        hoveredForStory =
            model.hoverStorySprint == Just ix && validStoryTarget

        hoveredForDelivery : Bool
        hoveredForDelivery =
            model.hoverDeliverySprint == Just ix && validDeliveryTarget

        -- 4) Visuals -------------------------------------------------
        baseClass : String
        baseClass =
            "min-w-0 px-3 py-2 rounded-xl border min-h-[84px] flex flex-col gap-1 transition "
                ++ cellBgColor

        -- ++ toneCls
        validCls : String
        validCls =
            if can && (validStoryTarget || validDeliveryTarget) then
                " border-indigo-300 border-dashed "

            else
                " border-slate-200 "

        hoverCls : String
        hoverCls =
            if can && (hoveredForStory || hoveredForDelivery) then
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
        storyHandlers : List (Html.Attribute GM.Msg)
        storyHandlers =
            if validStoryTarget then
                [ E.on "dragenter" (Json.Decode.succeed (GM.HoverStory (Just ix)))
                , E.preventDefaultOn "dragover" (Json.Decode.succeed ( GM.HoverStory (Just ix), True ))
                , E.on "dragleave" (Json.Decode.succeed (GM.HoverStory Nothing))
                , E.on "drop" (Json.Decode.succeed (GM.StoryDrop ix))
                ]

            else
                []

        deliveryHandlers : List (Html.Attribute GM.Msg)
        deliveryHandlers =
            if validDeliveryTarget then
                [ E.on "dragenter" (Json.Decode.succeed (GM.HoverDelivery (Just ix)))
                , E.preventDefaultOn "dragover" (Json.Decode.succeed ( GM.HoverDelivery (Just ix), True ))
                , E.on "dragleave" (Json.Decode.succeed (GM.HoverDelivery Nothing))
                , E.on "drop" (Json.Decode.succeed (GM.DeliveryDrop ix))
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
        emptyHint : List (Html GM.Msg)
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
                    [ featureCard toggles isGhostDelivery (GL.selectWarnKind row) can row ]

                else
                    []
               )
            ++ emptyHint
        )


storyCard : Bool -> Bool -> Story -> Html GM.Msg
storyCard isGhost can s =
    let
        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        dragAttrs : List (Html.Attribute GM.Msg)
        dragAttrs =
            if can then
                [ A.attribute "draggable" "true"
                , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
                , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
                , E.on "dragstart" (Json.Decode.succeed (GM.StoryDragStart s.id))
                , E.on "dragend" (Json.Decode.succeed GM.StoryDragEnd)
                ]

            else
                []
    in
    div
        ([ A.class
            ("w-[90%] min-w-0 flex items-center justify-between gap-2 pl-1 pr-2 py-1 rounded-lg border select-none "
                ++ cardToneForStatus s.status
                ++ ghostClass
            )
         , A.title s.title
         ]
            ++ dragAttrs
        )
        [ -- LEFT: rail + title (truncates nicely)
          div [ A.class "min-w-0 inline-flex items-center gap-1.5" ]
            (if can then
                [ dotRailPx (railSpec Small Regular)
                , span
                    [ A.class "font-medium truncate text-[13px]"
                    , E.onClick (GM.OpenWorkItem s.id)
                    ]
                    [ text s.title ]
                ]

             else
                [ span
                    [ A.class "font-medium truncate text-[13px]"
                    , E.onClick (GM.OpenWorkItem s.id)
                    ]
                    [ text s.title ]
                ]
            )
        ]


featureCard : Toggles -> Bool -> FeatureWarn -> Bool -> Feature -> Html GM.Msg
featureCard toggles isGhost warnKind can row =
    let
        -- visuals
        ghostClass =
            if isGhost then
                " opacity-30 saturate-50"

            else
                ""

        dragAttrs : List (Html.Attribute GM.Msg)
        dragAttrs =
            if can then
                [ A.attribute "draggable" "true"
                , A.attribute "ondragstart" "event.dataTransfer.effectAllowed='move'; event.dataTransfer.dropEffect='move'"
                , A.attribute "ondragover" "event.preventDefault(); event.dataTransfer.dropEffect='move'"
                , E.on "dragstart" (Json.Decode.succeed (GM.DeliveryDragStart row.featureId))
                , E.on "dragend" (Json.Decode.succeed GM.DeliveryDragEnd)
                ]

            else
                []
    in
    div
        (A.class
            ("mt-1 rounded-xl border-2 shadow-sm select-none overflow-hidden ring-1 ring-slate-200 "
                ++ cardToneForStatus row.status
                ++ ghostClass
            )
            :: A.title row.title
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
              div [ A.class "flex-1 pl-2 pr-3 py-2.5 min-w-0" ]
                [ -- row 1: title + status + warn
                  div [ A.class "flex items-center justify-between gap-2 min-w-0" ]
                    [ span
                        [ A.class "text-[13px] font-semibold truncate"
                        , E.onClick (GM.OpenWorkItem row.featureId)
                        ]
                        [ text row.title ]
                    ]
                , -- row 2: delivery + test chips
                  if warnKind == NoWarn then
                    div [ A.class "mt-1 flex items-center justify-between gap-2 min-w-0" ]
                        (statusPill Tiny row.status
                            :: maybe toggles.showTests (testStrip can toggles.editableTests row.featureId row.tests)
                        )

                  else
                    div [ A.class "flex items-center gap-1 shrink-0" ]
                        [ warnBadge Tiny warnKind ]
                ]
            ]
        ]


testStrip : Bool -> Bool -> Int -> Tests -> Html GM.Msg
testStrip can editableTests featureId tests =
    let
        mk label active kind =
            testChipView Tiny
                { label = label, active = active }
                -- HACK: Do not update tests
                (if can && editableTests then
                    [ E.onClick (GM.ToggleTest featureId kind) ]

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
