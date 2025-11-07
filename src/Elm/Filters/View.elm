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


lastSegment : String -> String
lastSegment p =
    p
        |> String.split "\\"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault p


view : Config.Config -> Bool -> FT.Model -> Html FM.Msg
view cfg tagsEnabled model =
    let
        panelCls =
            "relative z-30 overflow-visible mb-3 rounded-xl border border-indigo-200 bg-indigo-50/70 backdrop-blur px-3 py-2 shadow-sm"

        headerRow =
            div [ A.class "flex items-center justify-between mb-2" ]
                [ span [ A.class "text-sm font-semibold text-slate-700" ] [ text "Filters" ]
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
    in
    div [ A.class panelCls ]
        (headerRow
            :: (if not model.isOpen then
                    []

                else
                    [ controlsRow cfg tagsEnabled model ]
               )
         -- ++ [ debugFilters model.options model.sel.area model.sel.iteration ]
        )


controlsRow : Config.Config -> Bool -> FT.Model -> Html FM.Msg
controlsRow cfg tagsEnabled model =
    div [ A.class "grid grid-cols-1 md:grid-cols-3 gap-3" ]
        [ -- Area (ART) custom selector
          div []
            [ span [ A.class "block text-[11px] uppercase tracking-wide text-slate-500 mb-1" ] [ text "Area (ART)" ]

            -- , div [ A.class "w-[260px]" ]
            -- fixed width
            , AS.view model.areaSel |> Html.map FM.AreaSel
            ]
            -- ersätt tidigare PI-rendering
            , Html.div [ A.class "flex items-center justify-center" ]
                [ piPicker model ]
        , (if tagsEnabled then
            tagsField True cfg.tags model

           else
            tagsFieldSkeleton
           -- new tiny placeholder (or just an empty div)
          )
        ]


tagsFieldSkeleton : Html FM.Msg
tagsFieldSkeleton =
    div []
        [ span [ A.class "block text-[11px] uppercase tracking-wide text-slate-500 mb-1" ]
            [ text "Include tags" ]
        , div [ A.class "border border-slate-200 rounded-md px-2 py-1 bg-white text-slate-400" ]
            [ text "Filter tags…" ]
        ]



piPicker : FT.Model -> Html FM.Msg
piPicker model =
    let
        roots =
            model.options.iterations
    in
    case roots of
        [] ->
            -- inget att visa (ex. om vi ännu inte matat in Seed.piRoots i init)
            Html.span [ A.class "text-slate-400" ] [ text "No PIs" ]

        [ one ] ->
            -- edge case – visa bara en
            pill one

        _ ->
            -- visa de två första
            Html.div [ A.class "flex items-center gap-3" ]
                [ pill (List.head roots |> Maybe.withDefault "")
                , pill (List.drop 1 roots |> List.head |> Maybe.withDefault "")
                ]

pill : String -> Html FM.Msg
pill label =
    let
        cls =
            "px-4 py-2 rounded-full border border-slate-200 bg-white text-slate-700 cursor-pointer hover:bg-slate-50"
    in
    Html.span
        [ A.class cls
        , E.onClick (FM.SetIteration label)
        ]
        [ text label ]


tagsField : Bool -> Config.TagPolicy -> FT.Model -> Html FM.Msg
tagsField enabled tagPolicy model =
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

        disCls =
            if enabled then
                ""

            else
                " opacity-50 pointer-events-none select-none"

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
                , if enabled then
                    E.onClick (FM.ToggleTag t)

                  else
                    A.class "pointer-events-none"
                ]
                [ text t ]
    in
    div [ A.class disCls ]
        [ span [ A.class "block text-[11px] uppercase tracking-wide text-slate-500 mb-1" ] [ text "Include tags" ]
        , div [ A.class "mb-1 flex items-center gap-2" ]
            [ input
                [ A.class "flex-1 border border-slate-200 rounded-md px-2 py-1 bg-white"
                , A.placeholder "Filter tags…"
                , A.value model.ui.tagQuery
                , if enabled then
                    E.onInput FM.SetTagQuery

                  else
                    A.attribute "readonly" "true"
                ]
                []
            , button
                [ A.class "text-xs px-2 py-1 rounded-md border border-slate-200 hover:bg-slate-50"
                , if enabled then
                    E.onClick FM.ClearTagQuery

                  else
                    A.attribute "disabled" "true"
                ]
                [ text "Clear" ]
            ]
        , div [ A.class "flex flex-wrap gap-1" ] (List.map chip visibleTags)
        ]


debugFilters : { areas : List String, iterations : List String } -> Maybe String -> Maybe String -> Html msg
debugFilters opts selArea selPi =
    let
        show m =
            Maybe.withDefault "(Nothing)" m

        looksFull p =
            String.contains "\\" p

        flag m =
            case m of
                Just v ->
                    if looksFull v then
                        "(full)"

                    else
                        "(NOT full)"

                Nothing ->
                    ""
    in
    Html.div [ A.class "mt-2 text-[11px] text-slate-500" ]
        [ Html.text ("area selected = " ++ show selArea ++ " " ++ flag selArea)
        , Html.br [] []
        , Html.text ("pi selected   = " ++ show selPi ++ " " ++ flag selPi)
        , Html.br [] []
        , Html.text ("areas[0] = " ++ Maybe.withDefault "-" (List.head opts.areas))
        , Html.br [] []
        , Html.text ("iters[0] = " ++ Maybe.withDefault "-" (List.head opts.iterations))
        ]
