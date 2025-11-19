module Settings.View exposing (view)

import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes as A
import Html.Events as E
import Settings.Msg as SM
import Settings.Types as ST


view : ST.Model -> Html SM.Msg
view model =
    let
        panelCls =
            "relative z-20 overflow-visible mb-3 rounded-xl border border-slate-200 "
                ++ "bg-slate-50/70 backdrop-blur px-3 py-2 shadow-sm"
    in
    div [ A.class panelCls ]
        (viewHeader model
            :: (if model.isOpen then
                    [ viewBody model ]

                else
                    []
               )
        )


viewHeader : ST.Model -> Html SM.Msg
viewHeader model =
    div [ A.class "flex items-center justify-between mb-1" ]
        [ span [ A.class "text-sm font-semibold text-slate-700" ] [ text "Settings" ]
        , button
            [ A.class "text-xs px-2 py-1 rounded-md border border-slate-200 hover:bg-slate-50"
            , E.onClick SM.ToggleOpen
            ]
            [ text
                (if model.isOpen then
                    "Hide"

                 else
                    "Show"
                )
            ]
        ]


viewBody : ST.Model -> Html SM.Msg
viewBody model =
    div [ A.class "grid grid-cols-1 md:grid-cols-2 gap-4 mt-1" ]
        [ viewTestsSection model
        , viewTeamsSection model
        ]


viewTestsSection : ST.Model -> Html SM.Msg
viewTestsSection model =
    div [ A.class "flex flex-col gap-2" ]
        [ span [ A.class "text-[11px] uppercase tracking-wide text-slate-500" ]
            [ text "Tests / Quality tags" ]
        , label [ A.class "flex items-center gap-2 text-xs text-slate-700" ]
            [ input
                [ A.type_ "checkbox"
                , A.checked model.enableTests
                , E.onCheck SM.SetEnableTests
                ]
                []
            , text "Enable test chips on feature cards"
            ]
        , div [ A.class "grid grid-cols-3 gap-2 text-xs" ]
            [ smallTextInput "SIT tag" model.sitTag SM.SetSitTag
            , smallTextInput "UAT tag" model.uatTag SM.SetUatTag
            , smallTextInput "E2E tag" model.e2eTag SM.SetE2eTag
            ]
        , label [ A.class "flex items-center gap-2 text-xs text-slate-700" ]
            [ input
                [ A.type_ "checkbox"
                , A.checked model.editableTests
                , E.onCheck SM.ToggleEditableTests
                ]
                []
            , text "Test tags are editable"
            ]
        ]


viewTeamsSection : ST.Model -> Html SM.Msg
viewTeamsSection model =
    div [ A.class "flex flex-col gap-2" ]
        [ span [ A.class "text-[11px] uppercase tracking-wide text-slate-500" ]
            [ text "Teams / Ownership" ]
        , label [ A.class "flex items-center gap-2 text-xs text-slate-700" ]
            [ input
                [ A.type_ "checkbox"
                , A.checked model.enableTeamTags
                , E.onCheck SM.SetEnableTeamTags
                ]
                []
            , text "Enable team filter (team tags on features)"
            ]
        , div [ A.class "flex flex-col gap-1 text-xs" ]
            [ span [ A.class "text-[11px] text-slate-500" ]
                [ text "Team tags (comma separated)" ]
            , input
                [ A.class "border border-slate-200 rounded-md px-2 py-1 bg-white text-xs"
                , A.value model.teamTagsInput
                , E.onInput SM.SetTeamTagsInput
                , A.placeholder "TeamAccount, TeamAssort, TeamPortal"
                ]
                []
            ]
        ]


smallTextInput : String -> String -> (String -> SM.Msg) -> Html SM.Msg
smallTextInput labelTxt value toMsg =
    div [ A.class "flex flex-col gap-1" ]
        [ span [ A.class "text-[10px] uppercase tracking-wide text-slate-500" ]
            [ text labelTxt ]
        , input
            [ A.class "border border-slate-200 rounded-md px-2 py-1 bg-white text-xs"
            , A.value value
            , E.onInput toMsg
            ]
            []
        ]
