module Filters.View exposing (view)

import Config
import Filters.AreaSelector as AS
import Filters.Msg as FM
import Filters.Types as FT
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as A
import Html.Events as E
import Set
import String



-- PUBLIC VIEW ---------------------------------------------------------------


view : Config.Config -> FT.Model -> Html FM.Msg
view cfg model =
    let
        panelCls =
            "relative z-30 overflow-visible mb-3 rounded-xl border border-indigo-200 "
                ++ "bg-indigo-50/70 backdrop-blur px-3 py-2 shadow-sm"
    in
    div [ A.class panelCls ]
        (viewPanelHeader model
            :: (if model.isOpen then
                    [ viewPanelBody cfg model ]

                else
                    []
               )
        )


viewPanelHeader : FT.Model -> Html FM.Msg
viewPanelHeader model =
    div [ A.class "flex items-center justify-between mb-2" ]
        [ span [ A.class "text-sm font-semibold text-slate-700" ]
            [ text "Filters" ]
        , button
            [ A.class "text-xs px-2 py-1 rounded-md border border-slate-200 hover:bg-slate-50"
            , E.onClick FM.ToggleOpen
            ]
            [ text
                (if model.isOpen then
                    "Hide"

                 else
                    "Show"
                )
            ]
        ]


viewPanelBody : Config.Config -> FT.Model -> Html FM.Msg
viewPanelBody cfg model =
    div []
        [ viewTopRow cfg model
        , if model.ui.tagsOpen then
            viewTagDropdown cfg model

          else
            text ""
        ]



-- TOP ROW -------------------------------------------------------------------


viewTopRow : Config.Config -> FT.Model -> Html FM.Msg
viewTopRow cfg model =
    -- Vänster: Area. Höger: ett kluster med PI, Team, Tags
    div
        [ A.class "flex flex-wrap items-center justify-between gap-y-2" ]
        [ viewAreaSection model
        , div
            [ A.class "flex flex-wrap items-center gap-x-6 gap-y-2" ]
            [ viewIterationSection model
            , viewTeamPicker cfg model
            , viewTagTrigger model
            ]
        ]


viewAreaSection : FT.Model -> Html FM.Msg
viewAreaSection model =
    div [ A.class "flex flex-col gap-1 min-w-[220px]" ]
        [ span
            [ A.class "block text-[11px] uppercase tracking-wide text-slate-500" ]
            [ text "Area (ART)" ]
        , AS.view model.areaSel |> Html.map FM.AreaSel
        ]


viewIterationSection : FT.Model -> Html FM.Msg
viewIterationSection model =
    div [ A.class "flex flex-col gap-1 min-w-[220px]" ]
        [ span
            [ A.class "block text-[11px] uppercase tracking-wide text-slate-500" ]
            [ text "Iteration (PI)" ]
        , viewIterationPicker model.options.iterations model.sel.iteration
        ]


viewTagTrigger : FT.Model -> Html FM.Msg
viewTagTrigger model =
    let
        labelText =
            if model.ui.tagsOpen then
                "Hide tags"

            else
                "Show tags"
    in
    div [ A.class "flex flex-col gap-1 ml-3 lg:ml-6" ]
        [ span
            [ A.class "block text-[11px] uppercase tracking-wide text-slate-500" ]
            [ text "Include tags" ]
        , button
            [ A.type_ "button"
            , A.class
                ("inline-flex items-center justify-center px-4 h-9 rounded-full "
                    ++ "border border-slate-300 bg-white text-xs font-medium "
                    ++ "text-slate-600 hover:bg-slate-50"
                )
            , E.onClick FM.ToggleTagsOpen
            ]
            [ text labelText ]
        ]



-- PILLS (ITERATION + TEAM) --------------------------------------------------


pickerPillClass : Bool -> String
pickerPillClass isSelected =
    let
        base =
            "inline-flex items-center justify-center px-5 h-10 rounded-full "
                ++ "border text-[13px] font-medium transition-colors "
    in
    if isSelected then
        -- lite nedtonad, lik Flow Metrics
        base ++ "bg-indigo-50 border-indigo-300 text-indigo-700"

    else
        base ++ "bg-slate-50 border-slate-200 text-slate-700 hover:bg-slate-100"


type alias PickerPillProps msg =
    { label : String
    , selected : Bool
    , onClick : msg
    }


viewPickerPill : PickerPillProps FM.Msg -> Html FM.Msg
viewPickerPill { label, selected, onClick } =
    button
        [ A.type_ "button"
        , A.class (pickerPillClass selected)
        , E.onClick onClick
        ]
        [ text label ]


viewIterationPicker : List String -> Maybe String -> Html FM.Msg
viewIterationPicker piRoots selected =
    let
        mkLabel root =
            root
                |> String.split "\\"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault root
    in
    div [ A.class "flex flex-row flex-wrap gap-3 mt-1" ]
        (List.map
            (\root ->
                let
                    isSelected =
                        Just root == selected
                in
                viewPickerPill
                    { label = mkLabel root
                    , selected = isSelected
                    , onClick = FM.SetIteration root
                    }
            )
            piRoots
        )


viewTeamPicker : Config.Config -> FT.Model -> Html FM.Msg
viewTeamPicker cfg model =
    if not cfg.tags.enableTeamTags then
        text ""

    else
        let
            teams =
                cfg.tags.teamTags |> Set.toList |> List.sort

            selected =
                model.sel.team

            pillForTeam name =
                let
                    isSel =
                        selected == Just name
                in
                viewPickerPill
                    { label = name
                    , selected = isSel
                    , onClick = FM.SetTeam (Just name)
                    }

            allPill =
                viewPickerPill
                    { label = "All teams"
                    , selected = selected == Nothing
                    , onClick = FM.SetTeam Nothing
                    }
        in
        div [ A.class "flex flex-col gap-1 min-w-[220px]" ]
            [ span
                [ A.class "block text-[11px] uppercase tracking-wide text-slate-500" ]
                [ text "Team" ]
            , div
                [ A.class "flex flex-row flex-nowrap gap-3 overflow-x-auto pb-1" ]
                (allPill :: List.map pillForTeam teams)
            ]



-- TAGS: DROPDOWN + INNEHÅLL -------------------------------------------------


viewTagDropdown : Config.Config -> FT.Model -> Html FM.Msg
viewTagDropdown cfg model =
    let
        tagMode =
            model.sel.tagMode
    in
    div
        [ A.class "mt-3 border border-slate-200 rounded-lg bg-white px-3 py-2 shadow-sm" ]
        [ -- AND / OR högst upp i panelen
          div [ A.class "flex items-center justify-between mb-2 text-xs text-slate-600" ]
            [ span [] [ text "Tag match mode" ]
            , div [ A.class "flex items-center gap-2" ]
                [ tagModeButton "AND" FT.TagAnd tagMode
                , tagModeButton "OR" FT.TagOr tagMode
                ]
            ]
        , tagsField cfg.tags model
        ]


tagModeButton : String -> FT.TagMode -> FT.TagMode -> Html FM.Msg
tagModeButton label modeValue current =
    let
        isSel =
            current == modeValue
    in
    button
        [ A.type_ "button"
        , A.class
            ("px-3 h-8 rounded-full text-xs font-semibold border "
                ++ (if isSel then
                        "bg-slate-900 text-white border-slate-900"

                    else
                        "bg-white text-slate-700 border-slate-300 hover:bg-slate-50"
                   )
            )
        , E.onClick (FM.SetTagMode modeValue)
        ]
        [ text label ]


tagsField : Config.TagPolicy -> FT.Model -> Html FM.Msg
tagsField tagPolicy model =
    let
        q =
            String.toLower model.ui.tagQuery

        visibleTags : List String
        visibleTags =
            model.allTags
                |> List.filter (Config.tagVisibleForChooser tagPolicy)
                |> (if q == "" then
                        identity

                    else
                        List.filter (\t -> String.contains q (String.toLower t))
                   )

        chip : String -> Html FM.Msg
        chip t =
            let
                isOn =
                    Set.member t model.sel.includeTags

                cls =
                    if isOn then
                        "px-2 py-[2px] text-[11px] rounded-full border border-indigo-300 bg-indigo-50 text-indigo-700 cursor-pointer"

                    else
                        "px-2 py-[2px] text-[11px] rounded-full border border-slate-200 bg-white text-slate-500 hover:bg-slate-50 cursor-pointer"
            in
            span
                [ A.class cls
                , E.onClick (FM.ToggleTag t)
                ]
                [ text t ]
    in
    div []
        [ div [ A.class "mb-1 flex items-center gap-2" ]
            [ input
                [ A.class "flex-1 border border-slate-200 rounded-md px-2 py-1 bg-white"
                , A.placeholder "Filter tags…"
                , A.value model.ui.tagQuery
                , E.onInput FM.SetTagQuery
                ]
                []
            , button
                [ A.class "text-xs px-2 py-1 rounded-md border border-slate-200 hover:bg-slate-50"
                , E.onClick FM.ClearTagQuery
                ]
                [ text "Clear" ]
            ]
        , div [ A.class "mt-2 flex flex-wrap gap-2" ] (List.map chip visibleTags)
        ]
